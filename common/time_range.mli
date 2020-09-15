open! Core_kernel

type t =
  { lower_bound : Time_ns.Span.t option
  ; upper_bound : Time_ns.Span.t option
  }
[@@deriving sexp, bin_io, equal]

val range : Time_ns.Span.t option -> Time_ns.Span.t option -> t
val all : t
val is_all : t -> bool

(** Return -1 if the given value is less than the whole range, 0 if it's within the range
    (inclusive), or 1 if it's greater than the whole range. *)
val compare_point : Time_ns.Span.t -> t -> int

(** Return whether the range's lower bound is either [None] or [<= lower] and its upper
    bound is either [None] or [>= upper]. *)
val covers : t -> lower:Time_ns.Span.t -> upper:Time_ns.Span.t -> bool
