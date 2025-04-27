open! Core
open! Bonsai_web_proc
open Vdom_keyboard
open Memtrace_viewer_common

let wide_threshold_pixels = 1000

let app_state ~(data : Data.t Bonsai.Value.t) : App_state.t Bonsai.Computation.t =
  let open Bonsai.Let_syntax in
  let trie =
    let%map data in
    data.trie
  in
  App_state.component ~trie
;;

let info_panel ~(data : Data.t Bonsai.Value.t) : Vdom.Node.t Bonsai.Computation.t =
  let open Bonsai.Let_syntax in
  let info =
    let%map data in
    data.info
  in
  Info_panel.component ~info
;;

let poi_panel
  ~(app_state : App_state.t Bonsai.Value.t)
  ~(data : Data.t Bonsai.Value.t)
  ~(main_panel : Main_panel.t Bonsai.Value.t)
  : Poi_panel.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub { focus; _ } = Bonsai.read app_state in
  let%sub { trie; call_sites; hot_paths; hot_locations; _ } = Bonsai.read data in
  let%sub { set_focus; _ } = Bonsai.read main_panel in
  Poi_panel.component ~trie ~call_sites ~hot_paths ~hot_locations ~focus ~set_focus
;;

let filter_panel
  ~(data : Data.t Bonsai.Value.t)
  ~(server_state : Server_state.t Bonsai.Value.t)
  ~(inject_outgoing : (Action.t -> unit Vdom.Effect.t) Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub Data.
            { trie
            ; graph
            ; filtered_graph
            ; peak_allocations
            ; peak_allocations_time
            ; info
            ; _
            }
    =
    Bonsai.read data
  in
  let total_allocations =
    let%map data in
    data.total_allocations_unfiltered
  in
  let filtered_allocations =
    let%map trie and filtered_graph in
    let allocations_in_trie = Data.Fragment_trie.total_allocations trie in
    if Option.is_some filtered_graph then Some allocations_in_trie else None
  in
  let start_time =
    let%map info in
    Option.map info ~f:(fun (info : Data.Info.t) -> info.start_time)
    (* data.info should only be None when there's no data anyway *)
    |> Option.value ~default:Time_ns.epoch
  in
  Filter_panel.component
    ~graph
    ~filtered_graph
    ~total_allocations
    ~filtered_allocations
    ~peak_allocations
    ~peak_allocations_time
    ~start_time
    ~server_state
    ~inject_outgoing
;;

let component
  ~(data : Data.t Bonsai.Value.t)
  ~(server_state : Server_state.t Bonsai.Value.t)
  ~(inject_outgoing : (Action.t -> unit Vdom.Effect.t) Bonsai.Value.t)
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub info_panel_view = info_panel ~data in
  let%sub app_state = app_state ~data in
  let%sub main_panel = Main_panel.component ~data ~app_state in
  let%sub poi_panel = poi_panel ~main_panel ~data ~app_state in
  let%sub filter_panel_view = filter_panel ~data ~server_state ~inject_outgoing in
  let%sub window_size = Window_size.component in
  let filter_on_left =
    let%map { width; height = _ } = window_size in
    width > wide_threshold_pixels
  in
  let%sub left_bar =
    let nodes =
      let%map info_panel_view and filter_panel_view in
      [ info_panel_view; filter_panel_view ]
    in
    Sidebar.component
      nodes
      ~button_position:Right
      ~attr:(Value.return (Vdom.Attr.id "left-bar"))
  in
  let%sub right_bar =
    let nodes =
      let%map poi_panel in
      [ poi_panel.view ]
    in
    Sidebar.component
      nodes
      ~button_position:Left
      ~attr:(Value.return (Vdom.Attr.id "right-bar"))
  in
  return
    (let%mapn left_bar
     and right_bar
     and filter_on_left
     and info_panel_view
     and filter_panel_view
     and main_panel in
     let key_handler = main_panel.key_handler in
     let open Vdom in
     let div ?(attr = Attr.empty) id body = Node.div ~attrs:[ Attr.id id; attr ] body in
     div
       "app-container"
       ~attr:(Attr.on_keydown (Keyboard_event_handler.handle_or_ignore_event key_handler))
       (List.concat
          [ (if filter_on_left then [] else [ info_panel_view; filter_panel_view ])
          ; [ div
                "body-container"
                [ (if filter_on_left
                   then left_bar
                   else Node.none_deprecated [@alert "-deprecated"])
                ; main_panel.view
                ; right_bar
                ]
            ]
          ]))
;;
