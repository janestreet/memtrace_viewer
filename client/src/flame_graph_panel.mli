open! Core
open Bonsai_web_proc
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

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : Selection.t option
  ; reset_selection :
      Data.Fragment.t
      -> default_selection:App_state.Default_selection.t
      -> unit Vdom.Effect.t
  ; scroll_focus_into_view : unit Vdom.Effect.t
  }

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> call_sites:Data.Call_sites.t Bonsai.Value.t
  -> focus:Data.Fragment.t Bonsai.Value.t
  -> set_focus:
       (Data.Fragment.t
        -> default_selection:App_state.Default_selection.t
        -> unit Vdom.Effect.t)
         Bonsai.Value.t
  -> activate:(Selection.t -> unit Vdom.Effect.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
