open! Core
open! Bonsai_web_proc
open Memtrace_viewer_common

module Row = struct
  type t =
    { fragment : Data.Fragment.t
    ; display : Data.Location.t list
    ; allocations : Byte_units.Stable.V2.t
    }

  module Id = Data.Backtrace

  let compare (id1, t1) (id2, t2) =
    match Byte_units.compare t2.allocations t1.allocations with
    | 0 -> Data.Backtrace.compare id1 id2
    | c -> c
  ;;

  let id { fragment; _ } = Data.Fragment.backtrace fragment
end

module Table = struct
  module Row = Row

  module Col_id = struct
    module T = struct
      type t =
        | Location
        | Allocations
        | Percentage
      [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  include Bonsai_simple_table.Make (Row) (Col_id)
end

let view_location extra loc ~call_sites =
  let call_sites = Data.Call_sites.for_location call_sites loc in
  Vdom.Node.span
    ~attrs:[ Vdom.Attr.title (Data.Location.full_name loc ~call_sites:(Some call_sites)) ]
    [ Vdom.Node.text extra; Location.format_dom loc ~call_sites:None ]
;;

let cell ?(attr = Vdom.Attr.empty) node = { Table.node; attrs = [ attr ] }
let attr_if cond attr = if cond then attr else Vdom.Attr.empty

let location_col ~call_sites ~standard_attrs =
  let open Vdom in
  let header = cell (Node.text "Location") in
  let render _ (row : Table.Row.t) =
    let node =
      match row.display with
      | [] -> Node.div ~attrs:[ Attr.class_ "last-loc" ] [ Node.text "(empty)" ]
      | first :: rest ->
        (match List.rev rest with
         | [] ->
           Node.div
             ~attrs:[ Attr.class_ "last-loc" ]
             [ view_location "" first ~call_sites ]
         | last :: _ ->
           Node.span
             [ Node.div
                 ~attrs:[ Attr.class_ "last-loc" ]
                 [ view_location "" last ~call_sites ]
             ; Node.div
                 ~attrs:[ Attr.class_ "first-loc" ]
                 [ view_location "..." first ~call_sites ]
             ])
    in
    let attr = standard_attrs ~fragment:row.fragment in
    cell node ~attr
  in
  let classes = [ "locations-column" ] in
  let group = None in
  Table.Column.create ~classes ~header ~render ~group ()
;;

let special_class ~fragment =
  attr_if (Data.Fragment.is_trivial fragment) (Vdom.Attr.class_ "loc-special")
;;

let allocations_col ~standard_attrs =
  let header = cell (Vdom.Node.text "Allocs") in
  let render _id ({ fragment; allocations; _ } : Table.Row.t) =
    let node = Vdom.Node.text (Byte_units.Short.to_string allocations) in
    let attr = Vdom.Attr.many [ special_class ~fragment; standard_attrs ~fragment ] in
    cell ~attr node
  in
  let classes = [ "allocations-column" ] in
  let group = None in
  Table.Column.create ~classes ~header ~render ~group ()
;;

let percentage_col ~total_allocations ~standard_attrs =
  let header = cell (Vdom.Node.none_deprecated [@alert "-deprecated"]) in
  let render _id ({ fragment; allocations; _ } : Table.Row.t) =
    let percentage = Byte_units.(allocations // total_allocations) *. 100. in
    let node = Vdom.Node.textf "%.1f%%" percentage in
    let attr = Vdom.Attr.many [ special_class ~fragment; standard_attrs ~fragment ] in
    cell ~attr node
  in
  let classes = [ "percentage-column" ] in
  let group = None in
  Table.Column.create ~classes ~header ~render ~group ()
;;

let standard_attrs ~fragment ~focus ~set_focus ~on_click_row =
  (* Annoyingly, these have to be put on each column individually, because
     [Bonsai_simple_table] doesn't have support for putting attributes on rows *)
  let focus_class =
    attr_if
      (Data.Fragment.same fragment focus)
      (Vdom.Attr.class_ "location-table-fragment-in-focus")
  in
  let click_handler = Vdom.Attr.on_click (fun _ -> on_click_row fragment) in
  let double_click_handler = Vdom.Attr.on_double_click (fun _ -> set_focus fragment) in
  Vdom.Attr.many [ focus_class; click_handler; double_click_handler ]
;;

let cols ~total_allocations ~call_sites ~focus ~set_focus ~on_click_row =
  let standard_attrs ~fragment =
    standard_attrs ~fragment ~focus ~set_focus ~on_click_row
  in
  Table.Col_id.Map.of_alist_exn
    [ Location, location_col ~call_sites ~standard_attrs
    ; Allocations, allocations_col ~standard_attrs
    ; Percentage, percentage_col ~total_allocations ~standard_attrs
    ]
;;

let key_handler ~set_focus ~table_key_handler ~selected_fragment =
  let open Vdom_keyboard in
  let key = Keystroke.create' in
  match selected_fragment with
  | Some selected_fragment ->
    let set_focus_command : Keyboard_event_handler.Command.t =
      let keys = [ key Space; key Enter ] in
      let description = "Focus on selected" in
      let group = None in
      let handler _ =
        Effect.Many
          [ set_focus selected_fragment
          ; (* Don't scroll the table on Space *)
            Effect.Prevent_default
          ]
      in
      { keys; description; group; handler }
    in
    Keyboard_event_handler.add_command_exn table_key_handler set_focus_command
  | None ->
    let key_handler =
      Keyboard_event_handler.add_disabled_key_exn table_key_handler (key Space)
    in
    let key_handler =
      Keyboard_event_handler.add_disabled_key_exn key_handler (key Enter)
    in
    key_handler
;;

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : (Data.Backtrace.t * Data.Fragment.t) option
  ; set_selection : Data.Fragment.t option -> unit Vdom.Effect.t
  ; move_selection : [ `Prev | `Next ] -> unit Vdom.Effect.t
  }

let component
  ~total_allocations
  ~call_sites
  ~rows
  ~presorted
  ~focus
  ~set_focus
  ~on_click_row
  =
  let open Bonsai.Let_syntax in
  let%sub input : Table.Input.t Bonsai.Computation.t =
    let%arr focus
    and set_focus
    and on_click_row
    and total_allocations
    and call_sites
    and row_specs = rows
    and presorted in
    let row_specs = List.map ~f:(fun row -> Row.id row, row) row_specs in
    let row_specs =
      if presorted then row_specs else List.sort ~compare:Row.compare row_specs
    in
    let row_ids_in_order = `These (List.map ~f:fst row_specs) in
    let col_ids_in_order = [ Table.Col_id.Location; Allocations; Percentage ] in
    let rows = Data.Backtrace.Map.of_alist_exn row_specs in
    let cols = cols ~total_allocations ~call_sites ~focus ~set_focus ~on_click_row in
    let table_attrs = [ Vdom.Attr.class_ "location-table" ] in
    Table.Input.create ~rows ~cols ~row_ids_in_order ~col_ids_in_order ~table_attrs ()
  in
  let%sub table = Table.bonsai input in
  return
    (let%map { Table.Result.view
             ; view_for_testing = _
             ; key_handler = table_key_handler
             ; focus_row
             ; inject
             }
       =
       table
     and set_focus in
     let selection =
       Option.map focus_row ~f:(fun (backtrace, { fragment; _ }) -> backtrace, fragment)
     in
     let set_selection selection =
       inject (Set_focus_row (Option.map ~f:Data.Fragment.backtrace selection))
     in
     let move_selection dir = inject (Move_focus dir) in
     let key_handler =
       let selected_fragment = Option.map selection ~f:(fun (_, fragment) -> fragment) in
       key_handler ~table_key_handler ~set_focus ~selected_fragment
     in
     { view; key_handler; selection; set_selection; move_selection })
;;
