open! Core_kernel
open! Async_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : Data.Fragment.Oriented.t option
  ; fix_focus : new_zoom:Data.Fragment.t -> Vdom.Event.t
  }

val component
  :  data:Data.t Bonsai.Value.t
  -> orient:Data.Orientation.t Bonsai.Value.t
  -> zoom:Data.Fragment.t Bonsai.Value.t
  -> t Bonsai.Computation.t
