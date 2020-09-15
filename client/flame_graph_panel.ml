open! Core_kernel
open Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Keyboard_event_handler.t
  ; focus : (Data.Backtrace.t * Data.Entry.t) option
  ; fix_focus : Vdom.Event.t
  }
[@@deriving fields]

module Tree = struct
  type t = Data.Trie.t

  module Handle = Data.Backtrace

  module Node = struct
    type nonrec t =
      { tree : t
      ; location : Data.Location.t
      ; node : Data.Trie.Node.t
      ; prior_backtrace : Data.Backtrace.Reversed.t
      (** Backtrace of the node, *not* including its own location *)
      }

    let handle t =
      Data.Backtrace.of_reversed
        (Data.Backtrace.Reversed.cons t.location t.prior_backtrace)
    ;;

    let label t = Data.Location.defname t.location

    let size t =
      Data.Entry.allocations (Data.Trie.Node.entry t.node) |> Byte_units.bytes_float
    ;;

    let details t =
      let full_loc = Location.format_string t.location in
      let allocations = Data.Entry.allocations (Data.Trie.Node.entry t.node) in
      let percentage =
        100. *. Byte_units.(allocations // Data.Trie.total_allocations t.tree)
      in
      Format.sprintf
        "%s: %s (%.1f%%)"
        full_loc
        (allocations |> Byte_units.Short.to_string)
        percentage
    ;;

    let children t =
      let prior_backtrace = Data.Backtrace.Reversed.cons t.location t.prior_backtrace in
      List.map
        (Data.Trie.Node.children t.node |> Data.Location.Map.to_alist)
        ~f:(fun (location, child) ->
          { tree = t.tree; location; node = child; prior_backtrace })
    ;;
  end

  let roots t =
    List.map
      (Data.Location.Map.to_alist (Data.Trie.roots t))
      ~f:(fun (location, node) ->
        { Node.tree = t; location; node; prior_backtrace = Data.Backtrace.Reversed.nil })
  ;;

  let find_with_ancestry_rev t backtrace : (Node.t * Node.t list) option =
    let rec loop backtrace node location prior_backtrace ancestry_rev =
      let this_node = { Node.tree = t; node; location; prior_backtrace } in
      let ancestry_rev = this_node :: ancestry_rev in
      match backtrace with
      | [] -> Some (this_node, ancestry_rev)
      | next_loc :: backtrace ->
        let%bind.Option child =
          Data.Location.Map.find (Data.Trie.Node.children node) next_loc
        in
        let prior_backtrace = Data.Backtrace.Reversed.cons location prior_backtrace in
        loop backtrace child next_loc prior_backtrace ancestry_rev
    in
    match backtrace with
    | [] -> None
    | location :: backtrace ->
      let%bind.Option root = Data.Location.Map.find (Data.Trie.roots t) location in
      loop backtrace root location Data.Backtrace.Reversed.nil []
  ;;

  let find t backtrace = find_with_ancestry_rev t backtrace |> Option.map ~f:fst

  let find_ancestry t backtrace =
    find_with_ancestry_rev t backtrace
    |> Option.map ~f:(fun (_, nodes_rev) -> List.rev nodes_rev)
  ;;

  let find_parent t (node : Node.t) =
    find t (Data.Backtrace.of_reversed node.prior_backtrace)
  ;;

  let is_related _t ~strictly ~(ancestor : Node.t) ~(descendant : Node.t) =
    let handle_a = Node.handle ancestor in
    let handle_d = Node.handle descendant in
    List.is_prefix ~prefix:handle_a handle_d ~equal:Data.Location.equal
    && not (strictly && List.length handle_a = List.length handle_d)
  ;;
end

module View = Flame_graph_view.Make (Tree)

let component ~trie ~direction ~zoom =
  let open Bonsai.Let_syntax in
  let shape =
    match%map direction with
    | Filter.Explore_downwards_from_allocations -> Flame_graph_view.Shape.Icicles
    | Explore_upwards_from_main -> Flames
  in
  let width = Bonsai.Value.return 500 in
  let%sub view = View.component ~tree:trie ~shape ~width ~zoom in
  return
    (let%map trie = trie
     and view = view in
     let focus =
       match View.focus view with
       | None -> None
       | Some handle ->
         let%map.Option node = Data.Trie.find trie handle in
         handle, Data.Trie.Node.entry node
     in
     let key_handler = View.key_handler view in
     let view = View.view view in
     let fix_focus = Vdom.Event.Ignore in
     { view; key_handler; focus; fix_focus })
;;
