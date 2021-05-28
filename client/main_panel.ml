open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

module Zoom_state = struct
  type t = { zoom_backtrace : Data.Backtrace.t option } [@@deriving sexp, equal]
end

module type Tab_input = sig
  val data : Data.t Bonsai.Value.t
  val zoom : Data.Fragment.t Bonsai.Value.t
  val set_zoom : (Data.Fragment.t option -> Vdom.Event.t) Bonsai.Value.t
end

module Tab_extra = struct
  type t =
    { key_handler : Vdom_keyboard.Keyboard_event_handler.t
    ; focus : Data.Fragment.Oriented.t option
    ; fix_focus : new_zoom:Data.Fragment.t -> Vdom.Event.t
    ; orients_visible : Zoom_panel.Orientations.t
    ; which_interface : Zoom_panel.Which_interface.t
    }
end

module Tab (Input : Tab_input) = struct
  type t =
    | Flame_graph
    | Callee_table
    | Caller_table
  [@@deriving sexp, compare, enumerate, equal]

  module Extra = Tab_extra

  let table_panel ~orient ~select_tab:_ : (Vdom.Node.t * Extra.t) Bonsai.Computation.t =
    let open Bonsai.Let_syntax in
    let data = Input.data in
    let zoom = Input.zoom in
    let orient = Bonsai.Value.return orient in
    let%sub table_panel = Table_panel.component ~data ~zoom ~orient in
    return
      (let%map { Table_panel.focus; fix_focus; key_handler; view } = table_panel
       and orient = orient in
       ( view
       , { Extra.key_handler
         ; focus
         ; fix_focus
         ; orients_visible = Only orient
         ; which_interface = Table
         } ))
  ;;

  let orients_of_zoom zoom : Zoom_panel.Orientations.t =
    if Data.Location.is_toplevel (Data.Fragment.first zoom ~orient:Callers)
    then Only Callees
    else if Data.Location.is_allocation_site (Data.Fragment.last zoom ~orient:Callers)
    then Only Callers
    else Both
  ;;

  let flame_graph_panel ~select_tab:_ : (Vdom.Node.t * Extra.t) Bonsai.Computation.t =
    let open Bonsai.Let_syntax in
    let trie =
      let%map data = Input.data in
      data.trie
    in
    let%sub flame_graph_panel =
      Flame_graph_panel.component ~trie ~zoom:Input.zoom ~set_zoom:Input.set_zoom
    in
    return
      (let%map { Flame_graph_panel.view; key_handler; focus; fix_focus } =
         flame_graph_panel
       and zoom = Input.zoom in
       let orients_visible = orients_of_zoom zoom in
       ( view
       , { Extra.key_handler
         ; focus
         ; fix_focus
         ; orients_visible
         ; which_interface = Flame_graph
         } ))
  ;;

  let name = function
    | Callee_table -> "Callee Table"
    | Caller_table -> "Caller Table"
    | Flame_graph -> "Flame Graph"
  ;;

  let component = function
    | Callee_table -> table_panel ~orient:Callees
    | Caller_table -> table_panel ~orient:Callers
    | Flame_graph -> flame_graph_panel
  ;;

  let initial = Flame_graph

  let enabled =
    let open Bonsai.Let_syntax in
    return
      (let%map zoom = Input.zoom in
       fun tab ->
         match tab, orients_of_zoom zoom with
         | Callee_table, Only Callers -> false
         | Caller_table, Only Callees -> false
         | _ -> true)
  ;;
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : Data.Fragment.Oriented.t option
  ; fix_focus : new_zoom:Data.Fragment.t -> Vdom.Event.t
  ; orients_visible : Zoom_panel.Orientations.t
  ; which_interface : Zoom_panel.Which_interface.t
  }

let component
      ~(data : Data.t Bonsai.Value.t)
      ~(zoom : Data.Fragment.t Bonsai.Value.t)
      ~(set_zoom : (Data.Fragment.t option -> Vdom.Event.t) Bonsai.Value.t)
  : t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let module Tab_panel_input = struct
    let data = data
    let zoom = zoom
    let set_zoom = set_zoom
  end
  in
  let module Tab = Tab (Tab_panel_input) in
  let%sub tab_panel : (Tab.t, Tab_extra.t) Tab_panel.t Bonsai.Computation.t =
    Tab_panel.component (module Tab)
  in
  return
    (let%map { Tab_panel.view
             ; extra =
                 { Tab_extra.key_handler
                 ; focus
                 ; fix_focus
                 ; orients_visible
                 ; which_interface
                 }
             ; _
             }
      =
      tab_panel
     in
     let view =
       Vdom.Node.div
         [ Vdom.Attr.id "main-panel-container" ]
         [ Panel.panel ~id:"main-panel" view ]
     in
     { view; key_handler; focus; fix_focus; orients_visible; which_interface })
;;
