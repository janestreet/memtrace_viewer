open! Core_kernel
open Memtrace_viewer_common

module Event : sig
  type t = private
    | Alloc of
        { obj_id : Obj_id.t
        ; is_major : bool
        ; single_allocation_size : Byte_units.t
        ; nsamples : int
        ; size : Byte_units.t
        ; backtrace : Location.t list (* empty if [parse_backtraces] was false *)
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
val iter : t -> ?parse_backtraces:bool -> (Time_ns.Span.t -> Event.t -> unit) -> unit
