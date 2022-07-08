open! Core
open Memtrace_viewer_common

module Event : sig
  type t = Location.Code.t Event.t [@@deriving sexp_of]
end

type t

val of_memtrace_trace : Memtrace.Trace.Reader.t -> t
val info : t -> Data.Info.t
val iter : t -> parse_backtraces:bool -> (Time_ns.Span.t -> Event.t -> unit) -> unit
