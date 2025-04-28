open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

val component
  :  data:Data.t Bonsai.Value.t
  -> server_state:Server_state.t Bonsai.Value.t
  -> inject_outgoing:(Action.t -> unit Vdom.Effect.t) Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
