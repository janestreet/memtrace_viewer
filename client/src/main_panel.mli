open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

module Selection : sig
  module Flame_graph : sig
    type t =
      | Flame of
          { fragment : Data.Fragment.t
          ; extend_focus_to : unit Vdom.Effect.t
          }
      | Icicle of
          { fragment : Data.Fragment.t
          ; extend_focus_to : unit Vdom.Effect.t
          }
      | Focus of
          { location : Data.Location.t
          ; retract_callees_from_focus : unit Vdom.Effect.t option
          ; retract_callers_from_focus : unit Vdom.Effect.t option
          }
  end

  module Table : sig
    type t =
      { fragment : Data.Fragment.t
      ; extend_focus_to : unit Vdom.Effect.t option
      }
  end

  type t =
    | Flame_graph of { selection : Flame_graph.t option }
    | Table of
        { orient : Orientation.t
        ; selection : Table.t option
        ; retract_from_focus : unit Vdom.Effect.t option
        }

  val location : t -> Data.Location.t option
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : Selection.t
  ; set_focus : Data.Fragment.t -> unit Vdom.Effect.t
  }

val component
  :  data:Data.t Bonsai.Value.t
  -> app_state:App_state.t Bonsai.Value.t
  -> t Bonsai.Computation.t
