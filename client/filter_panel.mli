open! Core_kernel
open! Bonsai_web
open Memtrace_viewer_common

val component
  :  graph:Data.Graph.t Bonsai.Value.t
  -> filtered_graph:Data.Graph.t option Bonsai.Value.t
  -> filtered_allocations:Byte_units.t option Bonsai.Value.t
  -> start_time:Time_ns.t Bonsai.Value.t
  -> inject_outgoing:(Memtrace_viewer_common.Action.t -> Vdom.Event.t) Bonsai.Value.t
  -> server_state:Server_state.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
