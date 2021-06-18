open! Core
open Memtrace_viewer_common

module Event : sig
  type t = private
    | Alloc of
        { obj_id : Obj_id.t
        ; source : Memtrace.Trace.Allocation_source.t
        ; single_allocation_size : Byte_units.t
        ; nsamples : int
        ; size : Byte_units.t
        ; backtrace_buffer : Location.t array
        ; backtrace_length : int
        ; common_prefix : int
        }
    | Promote of Obj_id.t
    | Collect of Obj_id.t
    | End
end

type t

val create
  :  trace:Memtrace.Trace.Reader.t
  -> loc_cache:Location.Cache.t
  -> filter:Filter.t
  -> t

val trace : t -> Memtrace.Trace.Reader.t

module Mode : sig
  type t =
    | Preserve_backtraces
    (** Events will have backtraces but may not be in the right order and may not have
        accurate timestamps *)
    | Preserve_times
    (** Events will come in the correct order, with accurate times, but have empty
        backtraces *)
end

val iter : t -> mode:Mode.t -> (Time_ns.Span.t -> Event.t -> unit) -> unit
