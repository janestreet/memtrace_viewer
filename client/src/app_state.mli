open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

module Default_selection : sig
  type t =
    | First_caller
    | First_callee
    | No_selection
  [@@deriving sexp, equal]
end

type t =
  { focus : Data.Fragment.t
  ; set_focus :
      Data.Fragment.t
      -> default_selection:Default_selection.t
      -> reset_selection:
           (Data.Fragment.t -> default_selection:Default_selection.t -> unit Effect.t)
      -> unit Effect.t
  }

val component : trie:Data.Fragment_trie.t Value.t -> t Computation.t
