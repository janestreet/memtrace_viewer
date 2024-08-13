open! Core
open Range_intf

module Bound = struct
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

module Or_empty = struct
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

module Make (Point : Point) = struct
  module Point = Point

  type 'a range = 'a t =
    { lower_bound : 'a Bound.t
    ; upper_bound : 'a Bound.t
    }
  [@@deriving sexp, bin_io, equal, quickcheck]

  let valid_bounds (lower_bound : Point.t Bound.t) (upper_bound : Point.t Bound.t) =
    match lower_bound, upper_bound with
    | No_bound, _ | _, No_bound -> true
    | Closed lower_bound, Closed upper_bound -> Point.compare lower_bound upper_bound <= 0
    | Closed lower_bound, Open upper_bound
    | Open lower_bound, Closed upper_bound
    | Open lower_bound, Open upper_bound -> Point.compare lower_bound upper_bound < 0
  ;;

  let quickcheck_generator_range gen =
    Quickcheck.Generator.filter
      (quickcheck_generator_range gen)
      ~f:(fun { lower_bound; upper_bound } -> valid_bounds lower_bound upper_bound)
  ;;

  type t = Point.t range [@@deriving sexp, bin_io, equal, quickcheck]

  let range lower_bound upper_bound = { lower_bound; upper_bound }
  let at_least point = range (Closed point) No_bound
  let at_most point = range No_bound (Closed point)
  let greater_than point = range (Open point) No_bound
  let less_than point = range No_bound (Open point)
  let all = { lower_bound = No_bound; upper_bound = No_bound }

  let is_all = function
    | { lower_bound = No_bound; upper_bound = No_bound } -> true
    | _ -> false
  ;;

  module Which_bound = struct
    type t =
      | Lower
      | Upper
  end

  let in_bound ~(which : Which_bound.t) x (bound : Point.t Bound.t) =
    match which, bound with
    | _, No_bound -> true
    | Lower, Closed bound -> Point.compare x bound >= 0
    | Lower, Open bound -> Point.compare x bound > 0
    | Upper, Closed bound -> Point.compare x bound <= 0
    | Upper, Open bound -> Point.compare x bound < 0
  ;;

  let compare_point x { lower_bound; upper_bound } =
    if not (in_bound ~which:Lower x lower_bound)
    then -1
    else if not (in_bound ~which:Upper x upper_bound)
    then 1
    else 0
  ;;

  let compare_lower_bound (bound1 : Point.t Bound.t) (bound2 : Point.t Bound.t) =
    match bound1, bound2 with
    | No_bound, No_bound -> 0
    | No_bound, _ -> -1
    | _, No_bound -> 1
    | Closed bound1, Open bound2 when Point.equal bound1 bound2 -> -1
    | Open bound1, Closed bound2 when Point.equal bound1 bound2 -> 1
    | (Closed bound1 | Open bound1), (Closed bound2 | Open bound2) ->
      Point.compare bound1 bound2
  ;;

  let compare_upper_bound (bound1 : Point.t Bound.t) (bound2 : Point.t Bound.t) =
    match bound1, bound2 with
    | No_bound, No_bound -> 0
    | No_bound, _ -> 1
    | _, No_bound -> -1
    | Closed bound1, Open bound2 when Point.equal bound1 bound2 -> 1
    | Open bound1, Closed bound2 when Point.equal bound1 bound2 -> -1
    | (Closed bound1 | Open bound1), (Closed bound2 | Open bound2) ->
      Point.compare bound1 bound2
  ;;

  let covers_points { lower_bound; upper_bound } ~lower ~upper =
    in_bound ~which:Lower lower lower_bound && in_bound ~which:Upper upper upper_bound
  ;;

  let min_by ~f x y = if f x y < 0 then x else y
  let max_by ~f x y = if f x y > 0 then x else y

  let join
    { lower_bound = lower_bound1; upper_bound = upper_bound1 }
    { lower_bound = lower_bound2; upper_bound = upper_bound2 }
    =
    let lower_bound = min_by ~f:compare_lower_bound lower_bound1 lower_bound2 in
    let upper_bound = max_by ~f:compare_upper_bound upper_bound1 upper_bound2 in
    { lower_bound; upper_bound }
  ;;

  module Or_empty = struct
    open Or_empty

    type 'a or_empty = 'a t =
      | Non_empty of 'a range
      | Empty
    [@@deriving sexp, bin_io, equal, quickcheck]

    type t = Point.t or_empty [@@deriving sexp, bin_io, equal, quickcheck]

    let range lower_bound upper_bound = Non_empty { lower_bound; upper_bound }
    let at_least point = Non_empty (at_least point)
    let at_most point = Non_empty (at_most point)
    let greater_than point = Non_empty (greater_than point)
    let less_than point = Non_empty (less_than point)
    let all = Non_empty all

    let is_all t =
      match t with
      | Non_empty range -> is_all range
      | Empty -> false
    ;;

    let empty = Empty

    let is_empty t =
      match t with
      | Empty -> true
      | Non_empty _ -> false
    ;;

    let contains_point t point =
      match t with
      | Non_empty range -> compare_point point range = 0
      | Empty -> false
    ;;

    let join t1 t2 =
      match t1, t2 with
      | Empty, _ -> t2
      | _, Empty -> t1
      | Non_empty range1, Non_empty range2 -> Non_empty (join range1 range2)
    ;;

    let inter_ranges
      { lower_bound = lower_bound1; upper_bound = upper_bound1 }
      { lower_bound = lower_bound2; upper_bound = upper_bound2 }
      =
      let lower_bound = max_by ~f:compare_lower_bound lower_bound1 lower_bound2 in
      let upper_bound = min_by ~f:compare_upper_bound upper_bound1 upper_bound2 in
      if valid_bounds lower_bound upper_bound
      then Non_empty { lower_bound; upper_bound }
      else Empty
    ;;

    let inter t1 t2 =
      match t1, t2 with
      | _, Empty | Empty, _ -> Empty
      | Non_empty range1, Non_empty range2 -> inter_ranges range1 range2
    ;;

    let disjoint t1 t2 = is_empty (inter t1 t2)
  end

  let inter = Or_empty.inter_ranges

  let inter_opt t1 t2 =
    match inter t1 t2 with
    | Non_empty range -> Some range
    | Empty -> None
  ;;

  let disjoint t1 t2 = Or_empty.is_empty (inter t1 t2)
end

module Time_ns_span = Make (Time_ns.Span)

module Byte_units_as_point : Point with type t = Byte_units.t = struct
  include Byte_units
  include Byte_units.Stable.V2

  include
    Quickcheckable.Of_quickcheckable
      (Int63)
      (struct
        type t = Byte_units.t

        let of_quickcheckable = Byte_units.of_bytes_int63
        let to_quickcheckable = Byte_units.bytes_int63
      end)
end

module Byte_units = Make (Byte_units_as_point)
