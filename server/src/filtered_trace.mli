open! Core_kernel
open Memtrace_viewer_common

module Event : sig
  type t = private
    | Event of Memtrace.Trace.Event.t
    | End
end

type t

val create : trace:Memtrace.Trace.Reader.t -> filter:Filter.t -> t
val trace : t -> Memtrace.Trace.Reader.t
val iter : t -> ?parse_backtraces:bool -> (Time_ns.Span.t -> Event.t -> unit) -> unit
val bytes_of_nsamples : trace:t -> int -> Byte_units.t
