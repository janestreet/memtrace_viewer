open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t

val view : t -> Vdom.Node.t
val key_handler : t -> Vdom_keyboard.Keyboard_event_handler.t
val focus : t -> (Data.Backtrace.t * Data.Entry.t) option
val zoom : t -> Data.Backtrace.t option
val set_zoom : t -> Data.Backtrace.t option -> Vdom.Event.t
val component : data:Data.t Bonsai.Value.t -> t Bonsai.Computation.t
