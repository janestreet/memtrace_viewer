open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { poi : Data.Fragment.t
  ; set_poi : Data.Fragment.t -> Vdom.Event.t
  (** Set the current point of interest. As a side effect, resets the zoom to this new
      point of interest. *)
  ; zoom : Data.Fragment.t
  ; set_zoom : Data.Fragment.t option -> Vdom.Event.t
  }

val component : trie:Data.Fragment_trie.t Bonsai.Value.t -> t Bonsai.Computation.t
