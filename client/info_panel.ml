open! Core
open Bonsai_web
open Memtrace_viewer_common

let info_linef ?(attrs = []) pat =
  pat
  |> Printf.ksprintf (fun str ->
    Vdom.(Node.li ~attr:(Attr.many_without_merge attrs) [ Node.text str ]))
;;

let info_fieldf ?(attrs = []) label pat =
  pat
  |> Printf.ksprintf (fun str ->
    Vdom.(
      Node.li
        ~attr:(Attr.many_without_merge attrs)
        [ Node.span ~attr:(Attr.class_ "info-label") [ Node.textf "%s: " label ]
        ; Node.text str
        ]))
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
      | None -> info_linef "%s" (Filename.basename info.executable_name)
      | Some context -> info_linef "%s" context
    in
    Panel.panel
      ~id:"info-panel"
      ~title:"Summary"
      (Node.div
         ~attr:(Attr.class_ "summary")
         [ Node.ul
             ~attr:(Attr.class_ "info-fields")
             [ context_line
             ; info_fieldf
                 ~attrs:[ Attr.class_ "total-allocations" ]
                 "Total allocations"
                 "%s"
                 (total_allocations |> Byte_units.Short.to_string)
             ;
               info_fieldf
                 ~attrs:[ Attr.title info.executable_name ]
                 "Executable"
                 "%s"
                 (Filename.basename info.executable_name)
             ; info_fieldf "PID" "%Ld" info.pid
             ; info_fieldf "Host" "%s" info.host_name
             ; info_fieldf "Word size" "%d bits" info.word_size
             ; info_fieldf "Start time" "%a" print_timestamp info.start_time
             ; info_fieldf "Sample rate" "%g" info.sample_rate
             ]
         ])
;;

let component ~info ~total_allocations =
  let open Bonsai.Let_syntax in
  return
    (let%map info = info
     and total_allocations = total_allocations in
     view ~info ~total_allocations)
;;
