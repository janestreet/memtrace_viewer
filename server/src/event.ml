open! Core

module Source = struct
  type t = Memtrace.Trace.Allocation_source.t =
    | Minor
    | Major
    | External
  [@@deriving sexp_of]
end

type 'loc t =
  | Alloc of
      { obj_id : Obj_id.t
      ; source : Source.t
      ; single_allocation_size : Byte_units.t
      ; nsamples : int
      ; size : Byte_units.t
      ; backtrace_buffer : 'loc array
      ; backtrace_length : int
      ; common_prefix : int
      ; domain : Memtrace.Trace.Domain_id.t
      }
  | Promote of Obj_id.t
  | Collect of Obj_id.t
  | End

module As_sexp = struct
  type 'loc t =
    | Alloc of
        { obj_id : Obj_id.t
        ; source : Source.t
        ; single_allocation_size : Byte_units.t
        ; nsamples : int
        ; size : Byte_units.t
        ; backtrace : 'loc array
        ; common_prefix : int
        ; domain : int
        }
    | Promote of { obj_id : Obj_id.t }
    | Collect of { obj_id : Obj_id.t }
    | End
  [@@deriving sexp_of]
end

let as_sexp : 'loc t -> 'loc As_sexp.t = function
  | Alloc
      { obj_id
      ; source
      ; single_allocation_size
      ; nsamples
      ; size
      ; backtrace_buffer
      ; backtrace_length
      ; common_prefix
      ; domain
      } ->
    let backtrace = Array.sub backtrace_buffer ~pos:0 ~len:backtrace_length in
    let domain = (domain :> int) in
    Alloc
      { obj_id
      ; source
      ; single_allocation_size
      ; nsamples
      ; size
      ; backtrace
      ; common_prefix
      ; domain
      }
  | Promote obj_id -> Promote { obj_id }
  | Collect obj_id -> Collect { obj_id }
  | End -> End
;;

let sexp_of_t sexp_of_loc t = As_sexp.sexp_of_t sexp_of_loc (t |> as_sexp)
