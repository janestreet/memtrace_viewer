open! Core
open! Bonsai_web
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
      [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  include Bonsai_simple_table.Make (Row) (Col_id)
end

let view_location extra loc =
  Vdom.Node.span [ Vdom.Node.text extra; Location.format_dom loc ]
;;

let cell ?(attrs = []) node = { Table.node; attrs }

let location_col =
  let open Vdom in
  let header = cell (Node.text "Location") in
  let render _ (row : Table.Row.t) =
    let node =
      match row.display with
      | [] -> Node.div ~attr:(Attr.class_ "last-loc") [ Node.text "(empty)" ]
      | first :: rest ->
        (match List.rev rest with
         | [] -> Node.div ~attr:(Attr.class_ "last-loc") [ view_location "" first ]
         | last :: _ ->
           Node.span
             [ Node.div ~attr:(Attr.class_ "last-loc") [ view_location "" last ]
             ; Node.div ~attr:(Attr.class_ "first-loc") [ view_location "..." first ]
             ])
    in
    cell node
  in
  let classes = [ "locations-column" ] in
  let group = None in
  Table.Column.create ~classes ~header ~render ~group ()
;;

let allocations_col ~total_allocations =
  let header = cell (Vdom.Node.text "Allocations") in
  let render _id ({ allocations; _ } : Table.Row.t) =
    let percentage = Byte_units.(allocations // total_allocations) *. 100. in
    let node = Vdom.Node.textf !"%{Byte_units.Short} (%.1f%%)" allocations percentage in
    cell node
  in
  let classes = [ "allocations-column" ] in
  let group = None in
  Table.Column.create ~classes ~header ~render ~group ()
;;

let cols ~total_allocations =
  Table.Col_id.Map.of_alist_exn
    [ Location, location_col; Allocations, allocations_col ~total_allocations ]
;;

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : (Data.Backtrace.t * Data.Fragment.t) option
  ; set_selection : Data.Fragment.t option -> Vdom.Event.t
  ; move_selection : [ `Prev | `Next ] -> Vdom.Event.t
  }

let component ~total_allocations ~rows ~presorted =
  let open Bonsai.Let_syntax in
  let input : Table.Input.t Bonsai.Value.t =
    let%map total_allocations = total_allocations
    and row_specs = rows
    and presorted = presorted in
    let row_specs = List.map ~f:(fun row -> Row.id row, row) row_specs in
    let row_specs =
      if presorted then row_specs else List.sort ~compare:Row.compare row_specs
    in
    let row_ids_in_order = `These (List.map ~f:fst row_specs) in
    let col_ids_in_order = [ Table.Col_id.Location; Allocations ] in
    let rows = Data.Backtrace.Map.of_alist_exn row_specs in
    let cols = cols ~total_allocations in
    let table_attrs = [ Vdom.Attr.class_ "location-table" ] in
    { Table.Input.rows; cols; row_ids_in_order; col_ids_in_order; table_attrs }
  in
  let%sub table = Table.bonsai input in
  return
    (let%map { Table.Result.view; view_for_testing = _; key_handler; focus_row; inject } =
       table
     in
     let selection =
       Option.map focus_row ~f:(fun (backtrace, { fragment; _ }) -> backtrace, fragment)
     in
     let set_selection selection =
       inject (Set_focus_row (Option.map ~f:Data.Fragment.backtrace selection))
     in
     let move_selection dir = inject (Move_focus dir) in
     { view; key_handler; selection; set_selection; move_selection })
;;
