open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : Data.Fragment.Extension.t option
  ; set_focus : Data.Fragment.Extension.t option -> Vdom.Event.t
  ; move_focus : [ `Prev | `Next ] -> Vdom.Event.t
  }

module Row : sig
  type t =
    { backtrace : Data.Fragment.Extension.t
    ; allocations : Byte_units.t
    }
end

val component
  :  total_allocations:Byte_units.t Bonsai.Value.t
  -> rows:Row.t list Bonsai.Value.t
  -> presorted:bool Bonsai.Value.t
  -> only_show_last_frame:bool Bonsai.Value.t
  -> t Bonsai.Computation.t
