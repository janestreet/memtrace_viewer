open! Core
open Bonsai_web
open Memtrace_viewer_common

let info_linef ?attr pat =
  pat |> Printf.ksprintf (fun str -> Vdom.(Node.li ?attr [ Node.text str ]))
;;

let info_fieldf ?attr label pat =
  pat
  |> Printf.ksprintf (fun str ->
    Vdom.(
      Node.li
        ?attr
        [ Node.span ~attr:(Attr.class_ "info-label") [ Node.textf "%s: " label ]
        ; Node.text str
        ]))
;;

let print_timestamp () t =
  sprintf "%s (UTC)" (t |> Time_ns.to_sec_string ~zone:Time_float.Zone.utc)
;;

let panel_body ~(info : Data.Info.t option) =
  let open Vdom in
  match info with
  | None -> Node.none
  | Some info ->
    let context_line =
      match info.context with
      | Some context -> info_linef "%s" context
      | None -> Node.none
    in
    let word_size_in_bits = 8 * (info.word_size |> Byte_units.bytes_int_exn) in
    let sample_size = Byte_units.scale info.word_size (1.0 /. info.sample_rate) in
    Node.div
      ~attr:(Attr.class_ "summary")
      [ Node.ul
          ~attr:(Attr.class_ "info-fields")
          [ context_line
          ;
            info_fieldf
              ~attr:(Attr.title info.executable_name)
              "Executable"
              "%s"
              (Filename.basename info.executable_name)
          ; info_fieldf "PID" "%Ld" info.pid
          ; info_fieldf "Host" "%s" info.host_name
          ; info_fieldf "Word size" "%d bits" word_size_in_bits
          ; info_fieldf "Start time" "%a" print_timestamp info.start_time
          ; info_fieldf
              "Sample rate"
              !"%g (1/%{Byte_units.Short} bytes)"
              info.sample_rate
              sample_size
          ]
      ]
;;

let component ~info =
  let open Bonsai.Let_syntax in
  let panel_body =
    let%map info = info in
    panel_body ~info
  in
  let title =
    match%map (info : Data.Info.t option Value.t) with
    | Some info -> Filename.basename info.executable_name
    | None -> "Loading …"
  in
  Panel.panel
    ~title
    ~id:"info-panel"
    panel_body
    ~collapsible:(Yes { initial_state = Collapsed })
;;
