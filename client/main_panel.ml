open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

module Zoom_state = struct
  type t = { zoom : Data.Backtrace.t option } [@@deriving sexp, equal]
end

let zoom_state_holder =
  State_holder.component ~initial:{ Zoom_state.zoom = None } (module Zoom_state)
;;

module Tab = struct
  type t =
    | Flame_graph
    | Table
  [@@deriving sexp, compare, enumerate, equal]

  module Input = struct
    type t =
      { data : Data.t
      ; zoom : Data.Backtrace.t option
      }
    [@@deriving fields]
  end

  module Result = struct
    type t =
      { key_handler : Vdom_keyboard.Keyboard_event_handler.t
      ; focus : (Data.Backtrace.t * Data.Entry.t) option
      ; fix_focus : Vdom.Event.t
      }
  end

  let table_panel ~(input : Input.t Bonsai.Value.t) ~select_tab:_
    : (Vdom.Node.t * Result.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let data = Input.data <$> input in
    let zoom = Input.zoom <$> input in
    let%sub table_panel = Table_panel.component ~data ~zoom in
    return
      (let%map { Table_panel.focus; key_handler; view; fix_focus } = table_panel in
       view, { Result.key_handler; focus; fix_focus })
  ;;

  let flame_graph_panel ~(input : Input.t Bonsai.Value.t) ~select_tab:_
    : (Vdom.Node.t * Result.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let trie =
      let%map input = input in
      input.data.trie
    in
    let direction =
      let%map input = input in
      input.data.direction
    in
    let zoom =
      let%map input = input in
      input.zoom
    in
    let%sub flame_graph_panel = Flame_graph_panel.component ~trie ~direction ~zoom in
    return
      (let%map flame_graph_panel = flame_graph_panel in
       ( Flame_graph_panel.view flame_graph_panel
       , { Result.key_handler = Flame_graph_panel.key_handler flame_graph_panel
         ; focus = Flame_graph_panel.focus flame_graph_panel
         ; fix_focus = Flame_graph_panel.fix_focus flame_graph_panel
         } ))
  ;;

  let name = function
    | Table -> "Table"
    | Flame_graph -> "Flame Graph"
  ;;

  let component = function
    | Table -> table_panel
    | Flame_graph -> flame_graph_panel
  ;;

  let initial = Flame_graph
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : (Data.Backtrace.t * Data.Entry.t) option
  ; zoom : Data.Backtrace.t option
  ; set_zoom : Data.Backtrace.t option -> Vdom.Event.t
  }
[@@deriving fields]

let component ~(data : Data.t Bonsai.Value.t) : t Bonsai.Computation.t =
  let open Bonsai.Let_syntax in
  let%sub zoom_state_holder = zoom_state_holder in
  let tab_panel_input =
    let%map { current = { zoom }; _ } = zoom_state_holder
    and data = data in
    { Tab.Input.data; zoom }
  in
  let%sub tab_panel = Tab_panel.component (module Tab) tab_panel_input in
  return
    (let%map zoom_state_holder = zoom_state_holder
     and view, { Tab.Result.key_handler; focus; fix_focus } = tab_panel in
     let view = Vdom.Node.section [ Vdom.Attr.id "main-panel" ] [ view ] in
     let set_zoom new_zoom =
       Vdom.Event.Many [ zoom_state_holder.set { zoom = new_zoom }; fix_focus ]
     in
     let { Zoom_state.zoom } = zoom_state_holder.current in
     { view; key_handler; focus; zoom; set_zoom })
;;
