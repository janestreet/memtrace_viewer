open! Core
open! Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

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

let poi_panel ~(data : Data.t Bonsai.Value.t) ~(main_panel : Main_panel.t Bonsai.Value.t)
  : Poi_panel.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub { trie; hot_paths; hot_call_sites; _ } = Bonsai.read data in
  let%sub { set_focus; _ } = Bonsai.read main_panel in
  Poi_panel.component ~trie ~hot_paths ~hot_call_sites ~set_focus
;;

let explore_panel
      ~(data : Data.t Bonsai.Value.t)
      ~(poi_panel : Poi_panel.t Bonsai.Value.t)
      ~(main_panel : Main_panel.t Bonsai.Value.t)
  : Explore_panel.t Bonsai.Computation.t
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
  let%sub { Poi_panel.poi; orientations; _ } = Bonsai.read poi_panel in
  let%sub { selection; focus; set_focus; _ } = Bonsai.read main_panel in
  Explore_panel.component
    ~trie
    ~orientations
    ~selection
    ~poi
    ~focus
    ~set_focus
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
  let%sub main_panel = Main_panel.component ~data in
  let%sub poi_panel = poi_panel ~main_panel ~data in
  let%sub explore_panel = explore_panel ~data ~poi_panel ~main_panel in
  let%sub filter_panel_view = filter_panel ~data ~server_state ~inject_outgoing in
  return
    (let%mapn info_panel_view = info_panel_view
     and poi_panel = poi_panel
     and main_panel = main_panel
     and explore_panel = explore_panel
     and filter_panel_view = filter_panel_view in
     let key_handler =
       Vdom_keyboard.Keyboard_event_handler.merge
         ~on_dup:`Throw
         main_panel.key_handler
         explore_panel.key_handler
     in
     Vdom.Node.div
       ~attr:
         (Vdom.Attr.many_without_merge
            [ Vdom.Attr.id "container"; Vdom.Attr.on_keydown (onkeydown key_handler) ])
       [ info_panel_view
       ; explore_panel.view
       ; filter_panel_view
       ; poi_panel.view
       ; main_panel.view
       ])
;;
