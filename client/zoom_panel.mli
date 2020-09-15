open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t

val view : t -> Vdom.Node.t
val key_handler : t -> Vdom_keyboard.Keyboard_event_handler.t

val component
  :  data:Data.t Bonsai.Value.t
  -> focus:(Data.Backtrace.t * Data.Entry.t) option Bonsai.Value.t
  -> zoom:Data.Backtrace.t option Bonsai.Value.t
  -> set_zoom:(Data.Backtrace.t option -> Vdom.Event.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
