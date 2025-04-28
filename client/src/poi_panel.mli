open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

type t = { view : Vdom.Node.t }

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> call_sites:Data.Call_sites.t Bonsai.Value.t
  -> hot_paths:Data.Fragment.t list Bonsai.Value.t
  -> hot_locations:Data.Fragment.t list Bonsai.Value.t
  -> focus:Data.Fragment.t Bonsai.Value.t
  -> set_focus:(Data.Fragment.t -> unit Ui_effect.t) Bonsai.Value.t
  -> t Bonsai.Computation.t
