open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : Data.Fragment.Oriented.t option
  ; fix_focus : new_zoom:Data.Fragment.t -> Vdom.Event.t
  ; orients_visible : Zoom_panel.Orientations.t
  ; which_interface : Zoom_panel.Which_interface.t
  }

val component
  :  data:Data.t Bonsai.Value.t
  -> zoom:Data.Fragment.t Bonsai.Value.t
  -> set_zoom:(Data.Fragment.t option -> Vdom.Event.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
