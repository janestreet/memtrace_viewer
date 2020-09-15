open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

val component
  :  data:Data.t Bonsai.Value.t
  -> server_state:Server_state.t Bonsai.Value.t
  -> inject_outgoing:(Action.t -> Vdom.Event.t) Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
