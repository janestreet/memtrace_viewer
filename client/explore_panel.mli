open! Core
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  }

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> orientations:Poi_panel.Orientations.t Bonsai.Value.t
  -> selection:Main_panel.Selection.t Bonsai.Value.t
  -> poi:Data.Fragment.t Bonsai.Value.t
  -> focus:Data.Fragment.t Bonsai.Value.t
  -> set_focus:(Data.Fragment.t -> Vdom.Event.t) Bonsai.Value.t
  -> total_allocations:Byte_units.t Bonsai.Value.t
  -> t Bonsai.Computation.t
