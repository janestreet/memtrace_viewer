open! Core

type 'loc t =
  | Alloc of
      { obj_id : Obj_id.t
      ; source : Memtrace.Trace.Allocation_source.t
      ; single_allocation_size : Byte_units.t
      ; nsamples : int
      ; size : Byte_units.t
      ; backtrace_buffer : 'loc array
      ; backtrace_length : int
      ; common_prefix : int
      }
  | Promote of Obj_id.t
  | Collect of Obj_id.t
  | End
[@@deriving sexp_of]
