open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : (Data.Backtrace.t * Data.Fragment.t) option
  ; set_selection : Data.Fragment.t option -> unit Vdom.Effect.t
  ; move_selection : [ `Prev | `Next ] -> unit Vdom.Effect.t
  }

module Row : sig
  type t =
    { fragment : Data.Fragment.t
    ; display : Data.Location.t list
    ; allocations : Byte_units.t
    }
end

val component
  :  total_allocations:Byte_units.t Bonsai.Value.t
  -> call_sites:Data.Call_sites.t Bonsai.Value.t
  -> rows:Row.t list Bonsai.Value.t
  -> presorted:bool Bonsai.Value.t
  -> focus:Data.Fragment.t Bonsai.Value.t
  -> set_focus:(Data.Fragment.t -> unit Vdom.Effect.t) Bonsai.Value.t
  -> on_click_row:(Data.Fragment.t -> unit Vdom.Effect.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
