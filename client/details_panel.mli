open! Core_kernel
open! Bonsai_web
open Memtrace_viewer_common

val component
  :  focus:(Data.Backtrace.t * Data.Entry.t) option Bonsai.Value.t
  -> total_allocations:Byte_units.t Bonsai.Value.t
  -> direction:Filter.direction Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
