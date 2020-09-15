open! Core_kernel
open! Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

let main_panel ~(data : Data.t Bonsai.Value.t) : Main_panel.t Bonsai.Computation.t =
  Main_panel.component ~data
;;

let info_panel ~(data : Data.t Bonsai.Value.t) : Vdom.Node.t Bonsai.Computation.t =
  let open Bonsai.Let_syntax in
  let info =
    let%map data = data in
    data.info
  in
  let total_allocations =
    let%map data = data in
    data.total_allocations_unfiltered
  in
  Info_panel.component ~info ~total_allocations
;;

let help_panel ~(key_handler : Keyboard_event_handler.t Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  Help_panel.component ~key_handler
;;

let zoom_panel ~(data : Data.t Bonsai.Value.t) ~(main_panel : Main_panel.t Bonsai.Value.t)
  : Zoom_panel.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let focus = Main_panel.focus <$> main_panel in
  let zoom = Main_panel.zoom <$> main_panel in
  let set_zoom = Main_panel.set_zoom <$> main_panel in
  Zoom_panel.component ~data ~focus ~zoom ~set_zoom
;;

let filter_panel
      ~(data : Data.t Bonsai.Value.t)
      ~(server_state : Server_state.t Bonsai.Value.t)
      ~(inject_outgoing : (Action.t -> Vdom.Event.t) Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let graph =
    let%map data = data in
    data.graph
  in
  let filtered_graph =
    let%map data = data in
    data.filtered_graph
  in
  let filtered_allocations =
    let%map data = data in
    let allocations_in_trie = Data.Trie.total_allocations data.trie in
    if Option.is_some data.filtered_graph then Some allocations_in_trie else None
  in
  let start_time =
    let%map data = data in
    Option.map data.info ~f:(fun (info : Data.Info.t) -> info.start_time)
    (* data.info should only be None when there's no data anyway *)
    |> Option.value ~default:Time_ns.epoch
  in
  Filter_panel.component
    ~graph
    ~filtered_graph
    ~filtered_allocations
    ~start_time
    ~server_state
    ~inject_outgoing
;;

let details_panel
      ~(data : Data.t Bonsai.Value.t)
      ~(main_panel : Main_panel.t Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let focus = Bonsai.Value.map ~f:Main_panel.focus main_panel in
  let total_allocations =
    let%map data = data in
    Data.Trie.total_allocations data.trie
  in
  let direction =
    let%map data = data in
    data.direction
  in
  Details_panel.component ~focus ~total_allocations ~direction
;;

let onkeydown key_handler event =
  Keyboard_event_handler.handle_or_ignore_event key_handler event
;;

let component
      ~(data : Data.t Bonsai.Value.t)
      ~(server_state : Server_state.t Bonsai.Value.t)
      ~(inject_outgoing : (Action.t -> Vdom.Event.t) Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub info_panel_view = info_panel ~data in
  let%sub main_panel = main_panel ~data in
  let%sub zoom_panel = zoom_panel ~data ~main_panel in
  let%sub details_panel_view = details_panel ~data ~main_panel in
  let%sub filter_panel_view = filter_panel ~data ~server_state ~inject_outgoing in
  let key_handler =
    let%map main_panel = main_panel
    and zoom_panel = zoom_panel in
    Keyboard_event_handler.merge
      ~on_dup:`Throw
      (Zoom_panel.key_handler zoom_panel)
      (Main_panel.key_handler main_panel)
  in
  let%sub help_panel = help_panel ~key_handler in
  return
    (let%map data = data
     and info_panel_view = info_panel_view
     and help_panel_view = help_panel
     and zoom_panel_result = zoom_panel
     and main_panel_result = main_panel
     and filter_panel_view = filter_panel_view
     and details_panel_view = details_panel_view
     and key_handler = key_handler in
     let zoom_panel_view = Zoom_panel.view zoom_panel_result in
     let main_panel_view = Main_panel.view main_panel_result in
     let container_class =
       match data.direction with
       | Explore_downwards_from_allocations -> "exploring-downwards"
       | Explore_upwards_from_main -> "exploring-upwards"
     in
     let left_column =
       Vdom.Node.div [ Vdom.Attr.id "left-column" ] [ info_panel_view; help_panel_view ]
     in
     let middle_column =
       Vdom.Node.div [ Vdom.Attr.id "middle-column" ] [ zoom_panel_view; main_panel_view ]
     in
     Vdom.Node.div
       [ Vdom.Attr.id "container"
       ; Vdom.Attr.class_ container_class
       ; Vdom.Attr.on_keydown (onkeydown key_handler)
       ]
       [ left_column; middle_column; filter_panel_view; details_panel_view ])
;;
