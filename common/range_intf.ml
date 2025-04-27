open! Core

module type Point = sig
  type t [@@deriving sexp, bin_io, equal, compare, quickcheck]
end

module type Bound = sig
  type 'a t [@@deriving sexp, bin_io, equal, quickcheck]
end

module type S = sig
  module Point : Point
  module Bound : Bound

  type 'a range
  type 'a or_empty
  type t = Point.t range [@@deriving sexp, bin_io, equal, quickcheck]

  val range : Point.t Bound.t -> Point.t Bound.t -> t
  val at_least : Point.t -> t
  val at_most : Point.t -> t
  val greater_than : Point.t -> t
  val less_than : Point.t -> t
  val all : t
  val is_all : t -> bool

  (** Return the join of two ranges. This is an approximation to the setwise union, since
      the union of two disjoint intervals is not in general an interval. *)
  val join : t -> t -> t

  (** Return -1 if the given value is less than the whole range, 0 if it's within the
      range (inclusive), or 1 if it's greater than the whole range. *)
  val compare_point : Point.t -> t -> int

  (** Return whether the range's lower bound is either [No_bound] or less than [lower] and
      its upper bound is either [No_bound] or greater than [upper]. *)
  val covers_points : t -> lower:Point.t -> upper:Point.t -> bool

  val disjoint : t -> t -> bool

  module Or_empty : sig
    type nonrec t = Point.t or_empty [@@deriving sexp, bin_io, equal, quickcheck]

    val range : Point.t Bound.t -> Point.t Bound.t -> t
    val all : t
    val at_least : Point.t -> t
    val at_most : Point.t -> t
    val greater_than : Point.t -> t
    val less_than : Point.t -> t
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

module type Range = sig
  module Bound : sig
    type 'a t =
      | No_bound
      | Open of 'a
      | Closed of 'a
    [@@deriving sexp, bin_io, equal, quickcheck]
  end

  type 'a t =
    { lower_bound : 'a Bound.t
    ; upper_bound : 'a Bound.t
    }

  module Or_empty : sig
    type nonrec 'a t =
      | Non_empty of 'a t
      | Empty
  end

  module type Point = Point

  module type S =
    S
    with type 'a range := 'a t
    with type 'a or_empty := 'a Or_empty.t
    with module Bound := Bound

  module Make (Point : Point) : S with module Point = Point
  module Time_ns_span : S with type Point.t = Time_ns.Span.t
  module Byte_units : S with type Point.t = Byte_units.t
end
