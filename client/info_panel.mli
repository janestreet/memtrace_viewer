open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

val component
  :  info:Data.Info.t option Bonsai.Value.t
  -> total_allocations:Byte_units.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
