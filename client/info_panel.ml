open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

let info_linef ?(attrs = []) label pat =
  let open Vdom in
  let view str =
    let label_view =
      match label with
      | None -> Node.none
      | Some label -> Node.span [ Attr.class_ "info-label" ] [ Node.textf "%s: " label ]
    in
    Node.p attrs [ label_view; Node.text str ]
  in
  Printf.ksprintf view pat
;;

let print_timestamp () t =
  sprintf "%s (UTC)" (t |> Time_ns.to_sec_string ~zone:Time.Zone.utc)
;;

let view ~(info : Data.Info.t option) ~total_allocations =
  let open Vdom in
  match info with
  | None -> Node.none
  | Some info ->
    let context_line =
      match info.context with
      | None -> Node.none
      | Some context -> info_linef (Some "Context") "%s" context
    in
    Node.section
      [ Attr.id "info-panel" ]
      [ Node.h2 [] [ Node.text "Summary" ]
      ; info_linef
          ~attrs:[ Attr.class_ "total-allocations" ]
          None
          "Total allocations: %s"
          (total_allocations |> Byte_units.Short.to_string)
      ; info_linef
          ~attrs:[ Attr.title info.executable_name ]
          (Some "Executable")
          "%s"
          (Filename.basename info.executable_name)
      ; info_linef (Some "PID") "%Ld" info.pid
      ; info_linef (Some "Host") "%s" info.host_name
      ; info_linef (Some "Word size") "%d bits" info.word_size
      ; info_linef (Some "Start time") "%a" print_timestamp info.start_time
      ; info_linef (Some "Sample rate") "%g" info.sample_rate
      ; context_line
      ]
;;

let component ~info ~total_allocations =
  let open Bonsai.Let_syntax in
  return
    (let%map info = info
     and total_allocations = total_allocations in
     view ~info ~total_allocations)
;;
