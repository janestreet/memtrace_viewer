open! Core
open Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

module Graph = struct
  type t =
    { focus : Data.Fragment.t
    ; trie : Data.Fragment_trie.t
    ; flame_tree : Data.Fragment.Oriented.t list
    ; icicle_tree : Data.Fragment.Oriented.t list
    ; focus_seq : Data.Fragment.Iterator.t option
    }

  module Node = struct
    type t =
      | Tree of Data.Fragment.Oriented.t
      | Focus of Data.Fragment.Iterator.t

    let location = function
      | Tree tree -> Data.Fragment.Oriented.first tree
      | Focus seq -> Data.Fragment.Iterator.location seq
    ;;

    let label ~graph:_ t = Data.Location.defname (location t)

    let entry ~graph = function
      | Tree tree -> Data.Fragment.entry (Data.Fragment.Oriented.fragment tree)
      | Focus _ -> Data.Fragment.entry graph.focus
    ;;

    let allocations ~graph t = Data.Entry.allocations (entry ~graph t)
    let size ~graph t = allocations ~graph t |> Byte_units.bytes_float

    let details ~graph t =
      let loc = location t in
      let entry = entry ~graph t in
      String.concat
        [ Data.Location.full_name loc
        ; ": "
        ; Data.Entry.allocations_string entry
        ; " ("
        ; Data.Entry.percentage_string entry
        ; ")"
        ]
    ;;
  end

  module Tree = struct
    type t = Data.Fragment.Oriented.t

    let node t = Node.Tree t

    let children ~graph:_ t =
      let children = Data.Fragment.Oriented.one_frame_extensions t in
      List.filter_map children ~f:(fun (loc, fragment) ->
        if Data.Location.is_special loc then None else Some fragment)
    ;;

    let parent ~graph t =
      let%bind.Option t = Data.Fragment.Oriented.retract t in
      let fragment = Data.Fragment.Oriented.fragment t in
      if Data.Fragment.same fragment graph.focus then None else Some t
    ;;

    let same (t1 : t) (t2 : t) =
      let fragment1 = Data.Fragment.Oriented.fragment t1 in
      let fragment2 = Data.Fragment.Oriented.fragment t2 in
      Data.Fragment.same fragment1 fragment2
    ;;

    let allocations t =
      let fragment = Data.Fragment.Oriented.fragment t in
      let entry = Data.Fragment.entry fragment in
      Data.Entry.allocations entry
    ;;
  end

  module Sequence = struct
    type t = Data.Fragment.Iterator.t

    let node t = Node.Focus t

    let next ~graph:_ seq =
      let%bind.Option next = Data.Fragment.Iterator.next seq in
      if Data.Location.is_special (Data.Fragment.Iterator.location next)
      then None
      else Some next
    ;;

    let prev ~graph:_ seq =
      let%bind.Option prev = Data.Fragment.Iterator.prev seq in
      if Data.Location.is_special (Data.Fragment.Iterator.location prev)
      then None
      else Some prev
    ;;

    let same t1 t2 =
      let prefix1 = Data.Fragment.Iterator.prefix t1 in
      let prefix2 = Data.Fragment.Iterator.prefix t2 in
      Data.Fragment.same prefix1 prefix2
    ;;
  end

  let create ~trie ~focus =
    let flame_tree =
      if Data.Fragment.is_empty focus
      then []
      else (
        let orient = Data.Orientation.Callees in
        let tree = Data.Fragment.oriented focus ~orient in
        Tree.children ~graph:() tree)
    in
    let icicle_tree =
      if Data.Fragment.is_empty focus
      then []
      else (
        let orient = Data.Orientation.Callers in
        let tree = Data.Fragment.oriented focus ~orient in
        Tree.children ~graph:() tree)
    in
    let focus_seq =
      let%bind.Option seq = Data.Fragment.iterator_start focus in
      let rec loop seq =
        if Data.Location.is_special (Data.Fragment.Iterator.location seq)
        then (
          match Data.Fragment.Iterator.next seq with
          | None -> None
          | Some seq -> loop seq)
        else Some seq
      in
      loop seq
    in
    { trie; focus; flame_tree; icicle_tree; focus_seq }
  ;;

  let flame_tree { flame_tree; _ } = flame_tree
  let icicle_tree { icicle_tree; _ } = icicle_tree
  let focus { focus_seq; _ } = focus_seq

  let size { flame_tree; icicle_tree; focus_seq; focus; _ } =
    let allocations =
      match focus_seq with
      | Some _ ->
        let entry = Data.Fragment.entry focus in
        Data.Entry.allocations entry
      | None ->
        let flame_allocations =
          List.fold_left
            ~init:Byte_units.zero
            ~f:(fun acc tree -> Byte_units.(acc + Tree.allocations tree))
            flame_tree
        in
        let icicle_allocations =
          List.fold_left
            ~init:Byte_units.zero
            ~f:(fun acc tree -> Byte_units.(acc + Tree.allocations tree))
            icicle_tree
        in
        Byte_units.max flame_allocations icicle_allocations
    in
    Byte_units.bytes_float allocations
  ;;
end

module Flame_graph = Flame_graph_view.Make (Graph)

module Selection = struct
  type t =
    | Flame of { fragment : Data.Fragment.t }
    | Icicle of { fragment : Data.Fragment.t }
    | Focus of
        { callers_fragment : Data.Fragment.t
        ; callees_fragment : Data.Fragment.t
        }

  let of_selector (selector : Flame_graph.Selector.t) : t =
    match selector with
    | Flame tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      Flame { fragment }
    | Icicle tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      Icicle { fragment }
    | Focus seq ->
      let callers_fragment = Data.Fragment.Iterator.suffix seq in
      let callees_fragment = Data.Fragment.Iterator.prefix seq in
      Focus { callers_fragment; callees_fragment }
  ;;
end

module Default_selection = struct
  type t =
    | First_caller
    | First_callee
    | No_selection
  [@@deriving sexp, equal]
end

module State = struct
  type t =
    | Flame of { backtrace : Data.Backtrace.t }
    | Icicle of { backtrace : Data.Backtrace.Reversed.t }
    | Focus of { trace : Data.Fragment.Iterator.Trace.t }
  [@@deriving sexp, equal]

  let to_selector ~trie : t -> Flame_graph.Selector.t option = function
    | Flame { backtrace } ->
      let%map.Option fragment = Data.Fragment_trie.find trie backtrace in
      let orient = Data.Orientation.Callees in
      let tree = Data.Fragment.oriented fragment ~orient in
      Flame_graph.Selector.Flame tree
    | Icicle { backtrace } ->
      let%map.Option fragment = Data.Fragment_trie.find_rev trie backtrace in
      let orient = Data.Orientation.Callers in
      let tree = Data.Fragment.oriented fragment ~orient in
      Flame_graph.Selector.Icicle tree
    | Focus { trace } ->
      let%map.Option seq = Data.Fragment_trie.find_iterator trie trace in
      Flame_graph.Selector.Focus seq
  ;;

  let of_selector : Flame_graph.Selector.t -> t = function
    | Flame tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      let backtrace = Data.Fragment.backtrace fragment in
      Flame { backtrace }
    | Icicle tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      let backtrace = Data.Fragment.backtrace_rev fragment in
      Icicle { backtrace }
    | Focus seq ->
      let trace = Data.Fragment.Iterator.trace seq in
      Focus { trace }
  ;;

  let default ~state ~default_selection ~focus =
    match state with
    | None -> None
    | Some _ ->
      (match default_selection with
       | Default_selection.No_selection -> None
       | Default_selection.First_caller ->
         let%map.Option seq = Data.Fragment.iterator_start focus in
         let trace = Data.Fragment.Iterator.trace seq in
         Focus { trace }
       | Default_selection.First_callee ->
         let%map.Option seq = Data.Fragment.iterator_end focus in
         let trace = Data.Fragment.Iterator.trace seq in
         Focus { trace })
  ;;
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Keyboard_event_handler.t
  ; selection : Selection.t option
  ; reset_selection : Data.Fragment.t -> Default_selection.t -> Ui_event.t
  }

let component ~trie ~focus ~activate =
  let open Bonsai.Let_syntax in
  let width = Bonsai.Value.return 500 in
  let%sub state, set_state = Bonsai.state_opt [%here] (module State) in
  let%sub selection =
    return
      (let%map state = state
       and trie = trie in
       Option.bind ~f:(State.to_selector ~trie) state)
  in
  let%sub flame_graph =
    let set_selection =
      let%map set_state = set_state in
      fun selector -> set_state (Some (State.of_selector selector))
    in
    let activate =
      let%map activate = activate
      and set_state = set_state in
      fun selector ->
        Ui_event.Many
          [ set_state (Some (State.of_selector selector))
          ; activate (Selection.of_selector selector)
          ]
    in
    let graph : Graph.t Bonsai.Value.t =
      let%map focus = focus
      and trie = trie in
      Graph.create ~focus ~trie
    in
    Flame_graph.component
      graph
      ~width
      ~selection
      ~select:set_selection
      ~navigate_to:set_selection
      ~activate
  in
  let selection =
    let%map selection = selection in
    Option.map ~f:Selection.of_selector selection
  in
  return
    (let%map flame_graph = flame_graph
     and state = state
     and set_state = set_state
     and selection = selection in
     let view = flame_graph.view in
     let key_handler = flame_graph.key_handler in
     let reset_selection focus default_selection =
       let new_state = State.default ~state ~default_selection ~focus in
       set_state new_state
     in
     { view; key_handler; selection; reset_selection })
;;
