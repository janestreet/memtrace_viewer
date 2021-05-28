open! Core_kernel
open! Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

let main_panel
      ~(data : Data.t Bonsai.Value.t)
      ~(zoom_controller : Zoom_controller.t Bonsai.Value.t)
  : Main_panel.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub { Zoom_controller.zoom; set_zoom; _ } = Bonsai.read zoom_controller in
  Main_panel.component ~data ~zoom ~set_zoom
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

let poi_panel
      ~(data : Data.t Bonsai.Value.t)
      ~(zoom_controller : Zoom_controller.t Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub { trie; hot_paths; _ } = Bonsai.read data in
  let%sub { Zoom_controller.poi; set_poi; _ } = Bonsai.read zoom_controller in
  Poi_panel.component ~trie ~hot_paths ~poi ~set_poi
;;

let zoom_controller ~(data : Data.t Bonsai.Value.t)
  : Zoom_controller.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let trie =
    let%map data = data in
    data.trie
  in
  Zoom_controller.component ~trie
;;

let zoom_panel
      ~(data : Data.t Bonsai.Value.t)
      ~(zoom_controller : Zoom_controller.t Bonsai.Value.t)
      ~(main_panel : Main_panel.t Bonsai.Value.t)
  : Zoom_panel.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let trie =
    let%map data = data in
    data.trie
  in
  let total_allocations =
    let%map data = data in
    Data.Fragment_trie.total_allocations data.trie
  in
  let%sub { Zoom_controller.poi; set_poi; zoom; set_zoom } =
    Bonsai.read zoom_controller
  in
  let%sub { focus; fix_focus; orients_visible; which_interface; _ } =
    Bonsai.read main_panel
  in
  (* Awkwardly fit in a few hacks to keep the focus sensible after the zoom changes. This
     is tricky to do via [Bonsai.Edge.on_change] because we want the behavior to depend on
     the prior value of the focus, and at least I couldn't get that to work right. *)
  let set_zoom =
    let%mapn set_zoom = set_zoom
    and fix_focus = fix_focus
    and poi = poi in
    fun zoom ->
      Vdom.Event.Many
        [ set_zoom zoom; fix_focus ~new_zoom:(zoom |> Option.value ~default:poi) ]
  in
  Zoom_panel.component
    ~trie
    ~orients_visible
    ~which_interface
    ~focus
    ~poi
    ~set_poi
    ~zoom
    ~set_zoom
    ~total_allocations
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
    let allocations_in_trie = Data.Fragment_trie.total_allocations data.trie in
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
  let%sub zoom_controller = zoom_controller ~data in
  let%sub poi_panel_view = poi_panel ~data ~zoom_controller in
  let%sub main_panel = main_panel ~data ~zoom_controller in
  let%sub zoom_panel = zoom_panel ~data ~zoom_controller ~main_panel in
  let%sub filter_panel_view = filter_panel ~data ~server_state ~inject_outgoing in
  return
    (let%mapn info_panel_view = info_panel_view
     and poi_panel_view = poi_panel_view
     and main_panel = main_panel
     and zoom_panel = zoom_panel
     and filter_panel_view = filter_panel_view in
     let key_handler =
       Vdom_keyboard.Keyboard_event_handler.merge
         ~on_dup:`Throw
         main_panel.key_handler
         zoom_panel.key_handler
     in
     Vdom.Node.div
       [ Vdom.Attr.id "container"; Vdom.Attr.on_keydown (onkeydown key_handler) ]
       [ info_panel_view
       ; zoom_panel.view
       ; filter_panel_view
       ; poi_panel_view
       ; main_panel.view
       ])
;;
