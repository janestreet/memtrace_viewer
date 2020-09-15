module Ranges : sig
  type t =
    { live_range : Time_range.t
    ; allocated_range : Time_range.t
    }
  [@@deriving sexp, bin_io, equal]
end

type direction =
  | Explore_downwards_from_allocations
  | Explore_upwards_from_main
[@@deriving sexp, bin_io, equal]

type t =
  { ranges : Ranges.t
  ; direction : direction
  ; include_minor_heap : bool
  ; include_major_heap : bool
  }
[@@deriving sexp, bin_io, equal]

val default : t
val is_default : t -> bool
val always_true : t -> bool
