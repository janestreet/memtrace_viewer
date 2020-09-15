open! Core_kernel
open! Async_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : (Data.Backtrace.t * Data.Entry.t) option
  ; fix_focus : Vdom.Event.t
  }

val component
  :  data:Data.t Bonsai.Value.t
  -> zoom:Data.Backtrace.t option Bonsai.Value.t
  -> t Bonsai.Computation.t
