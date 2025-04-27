open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

val component : info:Data.Info.t option Bonsai.Value.t -> Vdom.Node.t Bonsai.Computation.t
