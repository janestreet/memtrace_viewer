open! Core_kernel

module type Point = sig
  type t [@@deriving sexp, bin_io, equal, compare, quickcheck]
end

module Bound = struct
  type 'a t =
    | No_bound
    | Open of 'a
    | Closed of 'a
  [@@deriving sexp, bin_io, equal, quickcheck]
end

module type Range = sig
  module Point : Point

  type t =
    { lower_bound : Point.t Bound.t
    ; upper_bound : Point.t Bound.t
    }
  [@@deriving sexp, bin_io, equal, quickcheck]

  val range : Point.t Bound.t -> Point.t Bound.t -> t
  val all : t
  val is_all : t -> bool

  (** Return the join of two ranges. This is an approximation to the setwise union, since
      the union of two disjoint intervals is not in general an interval. *)
  val join : t -> t -> t

  (** Return -1 if the given value is less than the whole range, 0 if it's within the
      range (inclusive), or 1 if it's greater than the whole range. *)
  val compare_point : Point.t -> t -> int

  (** Return whether the range's lower bound is either [No_bound] or less than [lower] and
      its
      upper bound is either [No_bound] or greater than [upper]. *)
  val covers_points : t -> lower:Point.t -> upper:Point.t -> bool

  val disjoint : t -> t -> bool

  module Or_empty : sig
    type nonrec t =
      | Non_empty of t
      | Empty
    [@@deriving sexp, bin_io, equal, quickcheck]

    val range : Point.t Bound.t -> Point.t Bound.t -> t
    val all : t
    val is_all : t -> bool
    val empty : t
    val is_empty : t -> bool
    val contains_point : t -> Point.t -> bool

    (** Return the join of two ranges. See caveat on [Make.join]. *)
    val join : t -> t -> t

    val inter : t -> t -> t
    val disjoint : t -> t -> bool
  end

  val inter : t -> t -> Or_empty.t
  val inter_opt : t -> t -> t option
end

module type S = sig
  module Bound = Bound

  module type Point = Point
  module type Range = Range

  module Make (Point : Point) : Range with module Point = Point
  module Time_ns_span : Range with type Point.t = Time_ns.Span.t
  module Byte_units : Range with type Point.t = Byte_units.t
end
