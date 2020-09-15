open! Core_kernel
open Memtrace_viewer_common

type t

val create : trace:Memtrace.Trace.Reader.t -> filter:Filter.t -> t
val trace : t -> Memtrace.Trace.Reader.t

val iter
  :  t
  -> ?parse_backtraces:bool
  -> (Memtrace.Trace.Timedelta.t -> Memtrace.Trace.Event.t -> unit)
  -> unit

val time_span_of_timedelta : Memtrace.Trace.Timedelta.t -> Time_ns.Span.t
