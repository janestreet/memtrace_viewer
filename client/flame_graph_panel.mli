open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t

val view : t -> Vdom.Node.t
val key_handler : t -> Vdom_keyboard.Keyboard_event_handler.t
val focus : t -> (Data.Backtrace.t * Data.Entry.t) option
val fix_focus : t -> Vdom.Event.t

val component
  :  trie:Data.Trie.t Bonsai.Value.t
  -> direction:Filter.direction Bonsai.Value.t
  -> zoom:Data.Backtrace.t option Bonsai.Value.t
  -> t Bonsai.Computation.t
