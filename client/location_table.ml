open! Core_kernel
open! Bonsai_web
open Memtrace_viewer_common

module Table = struct
  module Row = struct
    type t = { allocations : Byte_units.Stable.V2.t } [@@deriving sexp]

    module Id = Data.Fragment.Extension
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

  include Bonsai_simple_table.Make (Row) (Col_id)
end

let view_location extra loc =
  Vdom.Node.span [] [ Vdom.Node.text extra; Location.format_dom loc ]
;;

let cell ?(attrs = []) node = { Table.node; attrs }

let location_col ~only_show_last_frame =
  let open Vdom in
  let header = cell (Node.text "Location") in
  let render ({ callers; callees } : Table.Row.Id.t) _ =
    let node backtrace =
      match backtrace with
      | [] -> None
      | first :: rest ->
        (match List.rev rest with
         | [] -> Some (Node.div [ Attr.class_ "last-loc" ] [ view_location "" first ])
         | last :: _ ->
           if only_show_last_frame
           then Some (Node.div [ Attr.class_ "last-loc" ] [ view_location "" last ])
           else
             Some
               (Node.span
                  []
                  [ Node.div [ Attr.class_ "last-loc" ] [ view_location "" last ]
                  ; Node.div [ Attr.class_ "first-loc" ] [ view_location "..." first ]
                  ]))
    in
    let callers_node = node (callers |> Data.Backtrace.of_reversed) in
    let callees_node = node callees in
    let node =
      match List.filter_opt [ callers_node; callees_node ] with
      | [] -> Node.div [ Attr.class_ "last-loc" ] [ Node.text "(empty)" ]
      | [ node ] -> node
      | [ callers_node; callees_node ] ->
        (* Unlikely; generally the table will have either just callers or just callees of
           the zoom *)
        Node.div [] [ callees_node; Node.p [] []; callers_node ]
      | _ -> assert false
    in
    cell node
  in
  let group = None in
  Table.Column.create ~header ~render ~group ()
;;

let allocations_col ~total_allocations =
  let header = cell (Vdom.Node.text "Allocations") in
  let render _id ({ allocations } : Table.Row.t) =
    let percentage = Byte_units.(allocations // total_allocations) *. 100. in
    let node = Vdom.Node.textf !"%{Byte_units.Short} (%.1f%%)" allocations percentage in
    cell node
  in
  let group = None in
  Table.Column.create ~header ~render ~group ()
;;

let cols ~total_allocations ~only_show_last_frame =
  Table.Col_id.Map.of_alist_exn
    [ Location, location_col ~only_show_last_frame
    ; Allocations, allocations_col ~total_allocations
    ]
;;

module Row = struct
  type t =
    { backtrace : Data.Fragment.Extension.t
    ; allocations : Byte_units.t
    }

  let compare t1 t2 =
    match Byte_units.compare t2.allocations t1.allocations with
    | 0 -> Data.Fragment.Extension.compare t1.backtrace t2.backtrace
    | c -> c
  ;;
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : Data.Fragment.Extension.t option
  ; set_focus : Data.Fragment.Extension.t option -> Vdom.Event.t
  ; move_focus : [ `Prev | `Next ] -> Vdom.Event.t
  }

let component ~total_allocations ~rows ~presorted ~only_show_last_frame =
  let open Bonsai.Let_syntax in
  let input : Table.Input.t Bonsai.Value.t =
    let%map total_allocations = total_allocations
    and row_specs = rows
    and presorted = presorted
    and only_show_last_frame = only_show_last_frame in
    let rows =
      row_specs
      |> List.map ~f:(fun { Row.backtrace; allocations } ->
        backtrace, { Table.Row.allocations })
      |> Data.Fragment.Extension.Map.of_alist_exn
    in
    let row_specs =
      if presorted then row_specs else List.sort ~compare:Row.compare row_specs
    in
    let row_ids_in_order =
      `These (row_specs |> List.map ~f:(fun { Row.backtrace; _ } -> backtrace))
    in
    let col_ids_in_order = [ Table.Col_id.Location; Allocations ] in
    let cols = cols ~total_allocations ~only_show_last_frame in
    let table_attrs = [ Vdom.Attr.class_ "location-table" ] in
    { Table.Input.rows; cols; row_ids_in_order; col_ids_in_order; table_attrs }
  in
  let%sub table = Table.bonsai input in
  return
    (let%map { Table.Result.view; view_for_testing = _; key_handler; focus_row; inject } =
       table
     in
     let focus = focus_row |> Option.map ~f:(fun (focus, _) -> focus) in
     let set_focus focus = inject (Set_focus_row focus) in
     let move_focus dir = inject (Move_focus dir) in
     { view; key_handler; focus; set_focus; move_focus })
;;
