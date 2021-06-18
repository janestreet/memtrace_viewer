open! Core
open Bonsai_web
open Memtrace_viewer_common

module Orientations : sig
  type t =
    | Both
    | Only of Data.Orientation.t
end

type t =
  { view : Vdom.Node.t
  ; orientations : Orientations.t
  ; poi : Data.Fragment.t
  }

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> hot_paths:Data.Fragment.t list Bonsai.Value.t
  -> hot_call_sites:Data.Fragment.t list Bonsai.Value.t
  -> set_focus:(Data.Fragment.t -> Ui_event.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
