open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

module State = struct
  type t = { expanded_nodes : Data.Backtrace.Reversed.Set.t }
  [@@deriving sexp, equal, fields]

  let empty = { expanded_nodes = Data.Backtrace.Reversed.Set.empty }
end

module Action = struct
  type t =
    | Expand of Data.Backtrace.t
    | Collapse of Data.Backtrace.t
  [@@deriving sexp_of]
end

let state_machine =
  let apply_action ~inject:_ ~schedule_event:_ { State.expanded_nodes } = function
    | Action.Expand backtrace ->
      { State.expanded_nodes =
          Data.Backtrace.Reversed.Set.add
            expanded_nodes
            (backtrace |> Data.Backtrace.Reversed.of_forward)
      }
    | Collapse backtrace ->
      { expanded_nodes =
          Data.Backtrace.Reversed.Set.remove
            expanded_nodes
            (backtrace |> Data.Backtrace.Reversed.of_forward)
      }
  in
  let default_model = State.empty in
  Bonsai.state_machine0
    [%here]
    (module State)
    (module Action)
    ~apply_action
    ~default_model
;;

module Row = struct
  type t = Data.Trie.Node.t [@@deriving sexp]

  module Id = struct
    module T = struct
      type t =
        { percentage : float
        ; backtrace : Data.Backtrace.t
        }
      [@@deriving sexp]

      let compare t1 t2 =
        let c = Float.compare t2.percentage t1.percentage in
        if c = 0 then Data.Backtrace.compare t1.backtrace t2.backtrace else c
      ;;
    end

    include T
    include Comparable.Make (T)
  end
end

module Col_id = struct
  module T = struct
    type t =
      | Location
      | Allocations
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module Table = Bonsai_simple_table.Make (Row) (Col_id)

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : (Data.Backtrace.t * Data.Entry.t) option
  ; fix_focus : Vdom.Event.t
  }

let view_location extra loc =
  Vdom.Node.span [] [ Vdom.Node.text extra; Location.format_dom loc ]
;;

let cell ?(attrs = []) node = { Table.node; attrs }

let location_col ~(data : Data.t) ~zoom =
  let header = cell (Vdom.Node.text "Location") in
  let render (row_id : Row.Id.t) _ =
    let node =
      let have_valid_zoom =
        match zoom with
        | None -> false
        | Some zoom -> Option.is_some (Data.Trie.find data.trie zoom)
      in
      let zoom_length =
        match zoom, have_valid_zoom with
        | Some zoom, true -> List.length zoom
        | _ -> 0
      in
      (* The first entries of the backtrace will always match the zoom, so drop them *)
      assert (
        (not have_valid_zoom)
        || List.equal
             Data.Location.equal
             (List.take row_id.backtrace zoom_length)
             (zoom |> Option.value ~default:[]));
      let backtrace = List.drop row_id.backtrace zoom_length in
      match backtrace with
      | [] -> Vdom.Node.div [ Vdom.Attr.class_ "last-loc" ] [ Vdom.Node.text "???" ]
      | first :: rest ->
        (match List.rev rest with
         | [] -> Vdom.Node.div [ Vdom.Attr.class_ "last-loc" ] [ view_location "" first ]
         | last :: _ ->
           Vdom.Node.span
             []
             [ Vdom.Node.div [ Vdom.Attr.class_ "last-loc" ] [ view_location "" last ]
             ; Vdom.Node.div [ Vdom.Attr.class_ "first-loc" ] [ view_location "..." first ]
             ])
    in
    cell node
  in
  let group = None in
  Table.Column.create ~header ~render ~group ()
;;

let allocations_col ~expanded_nodes =
  let header = cell (Vdom.Node.text "Allocations") in
  let render ({ percentage; backtrace } : Row.Id.t) (node : Row.t) =
    let entry = Data.Trie.Node.entry node in
    let allocations =
      if Data.Backtrace.Reversed.Set.mem
           expanded_nodes
           (backtrace |> Data.Backtrace.Reversed.of_forward)
      then Data.Entry.allocations_excluding_children entry
      else Data.Entry.allocations entry
    in
    let node =
      Vdom.Node.textf "%s (%.1f%%)" (allocations |> Byte_units.Short.to_string) percentage
    in
    cell node
  in
  let group = None in
  Table.Column.create ~header ~render ~group ()
;;

let cols ~data ~zoom ~expanded_nodes =
  Col_id.Map.of_alist_exn
    [ Location, location_col ~data ~zoom; Allocations, allocations_col ~expanded_nodes ]
;;

let transform_input ~(data : Data.t) ~zoom ~state:{ State.expanded_nodes } =
  let total = Data.Trie.total_allocations data.trie in
  let rows =
    let mk_row ~backtrace_rev ~exclude_children node =
      let backtrace = Data.Backtrace.of_reversed backtrace_rev in
      let entry = Data.Trie.Node.entry node in
      let allocations =
        if exclude_children
        then Data.Entry.allocations_excluding_children entry
        else Data.Entry.allocations entry
      in
      let percentage = Byte_units.(allocations // total) *. 100. in
      let id = { Row.Id.percentage; backtrace } in
      id, node
    in
    let rec add_node ~backtrace_rev ~key:loc ~data:node rows =
      let backtrace_rev = Data.Backtrace.Reversed.cons loc backtrace_rev in
      let expanded = Data.Backtrace.Reversed.Set.mem expanded_nodes backtrace_rev in
      let rows =
        match expanded, Data.Entry.on_unroll (Data.Trie.Node.entry node) with
        | true, Hide -> rows
        | _, _ ->
          let exclude_children = expanded in
          let id, row = mk_row ~backtrace_rev ~exclude_children node in
          Row.Id.Map.add_exn ~key:id ~data:row rows
      in
      if expanded
      then add_nodes ~backtrace_rev (Data.Trie.Node.children node) rows
      else rows
    and add_nodes ~backtrace_rev map rows =
      Data.Location.Map.fold ~init:rows ~f:(add_node ~backtrace_rev) map
    in
    let zoom_node = Option.bind zoom ~f:(fun zoom -> Data.Trie.find data.trie zoom) in
    let roots =
      match zoom_node with
      | Some zoom_node -> Data.Trie.Node.children zoom_node
      | None -> Data.Trie.roots data.trie
    in
    let backtrace_rev =
      (* Check zoom_node here, since if it turns out that the zoomed backtrace doesn't
         exist, we should consistently act like it's not set at all *)
      match zoom, zoom_node with
      | Some zoom, Some _ -> Data.Backtrace.Reversed.of_forward zoom
      | _ -> Data.Backtrace.Reversed.nil
    in
    add_nodes ~backtrace_rev roots Row.Id.Map.empty
  in
  let cols = cols ~data ~zoom ~expanded_nodes in
  let row_ids = Row.Id.Map.keys rows in
  let row_ids_in_order = `These row_ids in
  let col_ids_in_order = [ Col_id.Location; Allocations ] in
  let table_attrs = [] in
  { Table.Input.rows; cols; row_ids_in_order; col_ids_in_order; table_attrs }
;;

let and_prevent_default event = Vdom.Event.Many [ Vdom.Event.Prevent_default; event ]

let unroll_action inject_action focus =
  let keystroke = Vdom_keyboard.Keystroke.create' ArrowRight in
  match focus with
  | Some (focused_backtrace, focused_row)
    when not (Data.Location.Map.is_empty (Data.Trie.Node.children focused_row)) ->
    let keys = [ keystroke ] in
    let description = "Expand focused item" in
    let group = None in
    let handler _ =
      inject_action (Action.Expand focused_backtrace) |> and_prevent_default
    in
    Keyboard_event_handler.Action.Command { keys; description; group; handler }
  | _ -> Disabled_key keystroke
;;

let reroll_action inject_action focus expanded_nodes =
  let keystroke = Vdom_keyboard.Keystroke.create' ArrowLeft in
  let disabled = Keyboard_event_handler.Action.Disabled_key keystroke in
  match focus with
  | None -> disabled
  | Some (focused_backtrace, _) ->
    (match List.rev focused_backtrace with
     | [] -> disabled
     | _ :: parent_backtrace_rev ->
       let has_expanded_parent =
         Data.Backtrace.Reversed.Set.mem
           expanded_nodes
           (parent_backtrace_rev |> Data.Backtrace.Reversed.of_reversed_list)
       in
       if has_expanded_parent
       then (
         let keys = [ keystroke ] in
         let description = "Collapse focused item" in
         let group = None in
         let handler _ =
           inject_action (Action.Collapse (List.rev parent_backtrace_rev))
           |> and_prevent_default
         in
         Keyboard_event_handler.Action.Command { keys; description; group; handler })
       else disabled)
;;

let transform_result
      ~state:{ State.expanded_nodes }
      ~inject_state_machine_action
      (res : Table.Result.t)
  =
  let view = res.view in
  let focus =
    match res.focus_row with
    | None -> None
    | Some ({ backtrace; _ }, entry) -> Some (backtrace, entry)
  in
  let key_handler =
    Keyboard_event_handler.merge
      ~on_dup:`Throw
      res.Table.Result.key_handler
      (Keyboard_event_handler.of_action_list_exn
         [ unroll_action inject_state_machine_action focus
         ; reroll_action inject_state_machine_action focus expanded_nodes
         ])
  in
  let view = Vdom.Node.div [ Vdom.Attr.id "table-panel" ] [ view ] in
  let fix_focus =
    match focus with
    | None -> Vdom.Event.Ignore
    | Some _ ->
      Vdom.Event.Many [ res.inject (Set_focus_row None); res.inject (Move_focus `Next) ]
  in
  let focus =
    Option.map focus ~f:(fun (backtrace, node) -> backtrace, Data.Trie.Node.entry node)
  in
  { view; key_handler; focus; fix_focus }
;;

let component ~data ~zoom =
  let open Bonsai.Let_syntax in
  let%sub state_machine = state_machine in
  let table_input =
    let%map data = data
    and zoom = zoom
    and state, _ = state_machine in
    transform_input ~data ~zoom ~state
  in
  let%sub table = Table.bonsai table_input in
  return
    (let%map state, inject_state_machine_action = state_machine
     and table = table in
     transform_result ~state ~inject_state_machine_action table)
;;
