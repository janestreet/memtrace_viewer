open! Core
open Bonsai_web.Proc
open Memtrace_viewer_common

val component : info:Data.Info.t option Bonsai.Value.t -> Vdom.Node.t Bonsai.Computation.t
