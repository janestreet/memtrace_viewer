open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Memtrace_viewer_common

let view ~focus ~total_allocations ~direction =
  let header_text = Vdom.Node.text "Selection" in
  let header = Vdom.Node.h2 [] [ header_text ] in
  let body =
    let to_percent size = 100. *. Byte_units.(size // total_allocations) in
    let location_line =
      let loc_name =
        match focus with
        | Some (locs, _) -> Data.Location.defname (List.last_exn locs)
        | None -> "\u{2013}"
        (* en dash *)
      in
      Vdom.Node.p [] [ Vdom.Node.text loc_name ]
    in
    let allocations_line =
      let value_part =
        match focus with
        | Some (_, entry) ->
          Vdom.Node.textf
            "%s (%.1f%%) / %s (%.1f%%) excluding children"
            (Data.Entry.allocations entry |> Byte_units.Short.to_string)
            (Data.Entry.allocations entry |> to_percent)
            (Data.Entry.allocations_excluding_children entry |> Byte_units.Short.to_string)
            (Data.Entry.allocations_excluding_children entry |> to_percent)
        | None -> Vdom.Node.text "\u{2013}"
      in
      Vdom.Node.p
        []
        [ Vdom.Node.span
            [ Vdom.Attr.class_ "info-label" ]
            [ Vdom.Node.text "Allocations: " ]
        ; value_part
        ]
    in
    let backtrace_view =
      match focus with
      | None -> Vdom.Node.none
      | Some (locs, _) -> Backtrace_view.render ~direction locs
    in
    Vdom.Node.div
      []
      [ location_line
      ; allocations_line
      ; Vdom.Node.p [ Vdom.Attr.class_ "info-label" ] [ Vdom.Node.text "Backtrace:" ]
      ; backtrace_view
      ]
  in
  Vdom.Node.section [ Vdom.Attr.id "details-panel" ] [ header; body ]
;;

let component ~focus ~total_allocations ~direction =
  let open Bonsai.Let_syntax in
  return
    (let%map focus = focus
     and total_allocations = total_allocations
     and direction = direction in
     view ~focus ~total_allocations ~direction)
;;
