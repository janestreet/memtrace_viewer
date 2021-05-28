open! Core_kernel
open Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Keyboard_event_handler.t
  ; focus : Data.Fragment.Oriented.t option
  ; fix_focus : new_zoom:Data.Fragment.t -> Vdom.Event.t
  }

module Node = struct
  type t =
    { trie : Data.Fragment_trie.t
    ; fragment : Data.Fragment.t
    }
end

let orient_of_shape (shape : Flame_graph_view.Shape.t) : Data.Orientation.t =
  match shape with
  | Flames -> Callees
  | Icicles -> Callers
;;

let shape_of_orient (orient : Data.Orientation.t) : Flame_graph_view.Shape.t =
  match orient with
  | Callees -> Flames
  | Callers -> Icicles
;;

module Tree :
  Flame_graph_view.Tree with type t = Data.Fragment_trie.t and type Node.t = Node.t =
struct
  type t = Data.Fragment_trie.t

  module Node = struct
    type nonrec t = Node.t =
      { trie : t
      ; fragment : Data.Fragment.t
      }

    let location ~shape t = Data.Fragment.first ~orient:(orient_of_shape shape) t.fragment
    let label ~shape t = Data.Location.defname (location ~shape t)

    let size t =
      Data.Entry.allocations (Data.Fragment.entry t.fragment) |> Byte_units.bytes_float
    ;;

    let hidden ~(shape : Flame_graph_view.Shape.t) t =
      Data.Location.is_special (location ~shape t)
    ;;

    let details ~shape t =
      let full_loc = Location.format_string (location ~shape t) in
      let allocations = Data.Entry.allocations (Data.Fragment.entry t.fragment) in
      let percentage =
        100. *. Byte_units.(allocations // Data.Fragment_trie.total_allocations t.trie)
      in
      Format.sprintf
        "%s: %s (%.1f%%)"
        full_loc
        (allocations |> Byte_units.Short.to_string)
        percentage
    ;;

    let children ~shape t =
      let children =
        Data.Fragment.one_frame_extensions t.fragment ~orient:(orient_of_shape shape)
      in
      List.map children ~f:(fun (_, child) -> { trie = t.trie; fragment = child })
    ;;

    let parent ~shape t =
      let parent = Data.Fragment.retract t.fragment ~orient:(orient_of_shape shape) in
      Option.map parent ~f:(fun parent -> { trie = t.trie; fragment = parent })
    ;;

    let same t1 t2 = Data.Fragment.same t1.fragment t2.fragment

    module Debug = struct
      type nonrec t = t

      let sexp_of_t { fragment; trie = _ } = Data.Fragment.Debug.sexp_of_t fragment
    end
  end

  let root t =
    let fragment = Data.Fragment_trie.empty_fragment t in
    { Node.trie = t; fragment }
  ;;

  let rec is_related
            t
            ~(shape : Flame_graph_view.Shape.t)
            ~strictly
            ~(ancestor : Node.t)
            ~(descendant : Node.t)
    =
    if Node.same ancestor descendant
    then not strictly
    else (
      match Node.parent ~shape descendant with
      | None -> false
      | Some descendant -> is_related t ~shape ~strictly:false ~ancestor ~descendant)
  ;;
end

module Flame_graph = Flame_graph_view.Make (Tree)

module Focus_state = struct
  type t =
    { orient : Data.Orientation.t
    ; backtrace : Data.Backtrace.t
    }
  [@@deriving sexp, equal]

  let of_oriented_fragment { Data.Fragment.Oriented.orient; fragment } =
    let backtrace = Data.Fragment.backtrace fragment in
    { orient; backtrace }
  ;;

  let to_oriented_fragment { orient; backtrace } ~trie =
    let%map.Option fragment = Data.Fragment_trie.find trie backtrace in
    { Data.Fragment.Oriented.orient; fragment }
  ;;
end

module State = struct
  type t = { focus_state : Focus_state.t option } [@@deriving sexp, equal]

  let default = { focus_state = None }
end

let component ~trie ~zoom ~set_zoom =
  let open Bonsai.Let_syntax in
  let width = Bonsai.Value.return 500 in
  let%sub { State.focus_state }, set_state =
    Bonsai.state [%here] (module State) ~default_model:State.default
  in
  let focus_as_oriented_fragment =
    let%map focus_state = focus_state
    and trie = trie in
    let%bind.Option focus_state = focus_state in
    Focus_state.to_oriented_fragment focus_state ~trie
  in
  let set_focus_to_oriented_fragment =
    let%map set_state = set_state in
    fun focus ->
      let focus_state = Option.map ~f:Focus_state.of_oriented_fragment focus in
      set_state { focus_state }
  in
  let%sub flame_graph =
    let focus =
      let%map focus = focus_as_oriented_fragment
      and trie = trie in
      let%map.Option { Data.Fragment.Oriented.orient; fragment } = focus in
      { Flame_graph.Node.shape = shape_of_orient orient; tree_node = { fragment; trie } }
    in
    let set_focus =
      let%map set_focus_to_oriented_fragment = set_focus_to_oriented_fragment in
      fun (focus : Flame_graph.Node.t option) ->
        let oriented_fragment : Data.Fragment.Oriented.t option =
          focus
          |> Option.map ~f:(fun { shape; tree_node = { fragment; trie = _ } } ->
            let orient = orient_of_shape shape in
            { Data.Fragment.Oriented.orient; fragment })
        in
        set_focus_to_oriented_fragment oriented_fragment
    in
    let zoom =
      let%map trie = trie
      and zoom = zoom in
      { Node.trie; fragment = zoom }
    in
    let set_zoom =
      let%map set_zoom = set_zoom in
      fun zoom -> set_zoom (Option.map zoom ~f:(fun zoom -> zoom.Node.fragment))
    in
    Flame_graph.component ~tree:trie ~width ~focus ~set_focus ~zoom ~set_zoom
  in
  return
    (let%map trie = trie
     and flame_graph = flame_graph
     and focus_state = focus_state
     and set_focus_to_oriented_fragment = set_focus_to_oriented_fragment in
     let key_handler = flame_graph.key_handler in
     let focus =
       let%bind.Option focus_state = focus_state in
       Focus_state.to_oriented_fragment focus_state ~trie
     in
     let fix_focus ~new_zoom =
       (* This function is called when the zoom changes. Its job is to maintain, if
          feasible, the invariant that the focus extends the zoom (which is to say, it
          appears in the zoom's flame or icicle graph). Trickiness happens when the focus
          needs to move from one graph to the other.

          In the most common case, the focus is in the icicle graph and it becomes part of
          the zoom (and thus will flip to the flame graph). This usually happens when the
          user clicks Expand (or double-clicks), which sets the focus to the zoom, so it
          (mostly) suffices to check whether the new zoom is exactly the old focus but in
          the icicle graph (caller-oriented).

          In this case, we note that new focus should be at the bottom of the new zoom
          (= the old focus), so we take the bottom frame of the old focus and take its
          singleton fragment to be the new focus.

          {v
                 <- callers                                callees ->
              ... -------- icicles -------|------- old zoom -------|-- flames -- ...
                 .    .    .    .    .    .    .    .    .    .    .
                                |**** ------- old focus -----------|
              ... --- icicles --|------------ new zoom ------------|-- flames -- ...
                                |****|
                               new focus

              **** = frame whose location is displayed in the graph (first caller if
                     icicle, last callee if flame/zoom)
           v}
       *)

       let expanding_into_icicle_graph { Data.Fragment.Oriented.orient; fragment = focus }
         =
         Data.Orientation.equal orient Callers && Data.Fragment.same focus new_zoom
       in
       match focus with
       | Some focus when expanding_into_icicle_graph focus ->
         let focus =
           Data.Fragment_trie.find
             trie
             [ Data.Fragment.first focus.fragment ~orient:Callers ]
           |> Option.value_exn
         in
         set_focus_to_oriented_fragment (Some { orient = Callees; fragment = focus })
       | _ -> Vdom.Event.Ignore
     in
     let view = flame_graph.view in
     { view; key_handler; focus; fix_focus })
;;
