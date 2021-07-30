open! Core
open Bonsai_web
open Memtrace_viewer_common

module Selection : sig
  type t =
    | Flame of { fragment : Data.Fragment.t }
    | Icicle of { fragment : Data.Fragment.t }
    | Focus of
        { callers_fragment : Data.Fragment.t
        ; callees_fragment : Data.Fragment.t
        }
end

module Default_selection : sig
  type t =
    | First_caller
    | First_callee
    | No_selection
  [@@deriving sexp, equal]
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : Selection.t option
  ; reset_selection : Data.Fragment.t -> Default_selection.t -> unit Vdom.Effect.t
  }

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> focus:Data.Fragment.t Bonsai.Value.t
  -> activate:(Selection.t -> unit Vdom.Effect.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
