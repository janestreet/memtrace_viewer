open! Core

type t =
  { allocations : Byte_units.t
  ; time : Time_ns.Span.t
  }

val find_peak_allocations : Raw_trace.t -> t
