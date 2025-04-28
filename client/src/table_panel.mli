open! Core
open! Async_kernel
open Bonsai_web_proc
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : Data.Fragment.t option
  ; reset_selection : unit -> unit Ui_effect.t
  }

val component
  :  data:Data.t Bonsai.Value.t
  -> orient:Orientation.t Bonsai.Value.t
  -> focus:Data.Fragment.t Bonsai.Value.t
  -> set_focus:(Data.Fragment.t -> unit Ui_effect.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
