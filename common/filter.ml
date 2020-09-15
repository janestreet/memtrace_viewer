open! Core_kernel

module Ranges = struct
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

let default =
  { ranges = { allocated_range = Time_range.all; live_range = Time_range.all }
  ; direction = Explore_downwards_from_allocations
  ; include_minor_heap = true
  ; include_major_heap = true
  }
;;

let always_true = function
  | { ranges = { live_range; allocated_range }
    ; direction = _
    ; include_minor_heap
    ; include_major_heap
    } ->
    Time_range.is_all live_range
    && Time_range.is_all allocated_range
    && include_minor_heap
    && include_major_heap
;;

let is_default t =
  always_true t && equal_direction t.direction Explore_downwards_from_allocations
;;
