open! Core_kernel

type t = Memtrace.Trace.Obj_id.t

include Hashable.S_plain with type t := t
