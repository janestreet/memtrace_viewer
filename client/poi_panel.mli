open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> hot_paths:Data.Fragment.t list Bonsai.Value.t
  -> poi:Data.Fragment.t Bonsai.Value.t
  -> set_poi:(Data.Fragment.t -> Vdom.Event.t) Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
