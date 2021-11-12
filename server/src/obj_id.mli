open! Core

type t = Memtrace.Trace.Obj_id.t [@@deriving sexp_of]

include Hashable.S_plain with type t := t
