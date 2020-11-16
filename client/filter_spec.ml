open! Core_kernel
open Memtrace_viewer_common

module Range_predicate = struct
  type 'a t =
    | At_least of 'a option
    | At_most of 'a option
    | Between of
        { lower_bound : 'a option
        ; upper_bound : 'a option
        }
  [@@deriving sexp, equal]

  module To_range (R : Range.Range) = struct
    let to_range (t : R.Point.t t) : R.t option =
      match t with
      | At_least (Some lower_bound) -> Some (R.range (Closed lower_bound) No_bound)
      | At_most (Some upper_bound) -> Some (R.range No_bound (Closed upper_bound))
      | Between { lower_bound = Some lower_bound; upper_bound = Some upper_bound } ->
        Some (R.range (Closed lower_bound) (Closed upper_bound))
      | _ -> None
    ;;
  end

  module Or_empty = struct
    type nonrec 'a t =
      | Non_empty of 'a t
      | Empty
    [@@deriving sexp, equal]

    module To_range (R : Range.Range) = struct
      let to_range (t : R.Point.t t) : R.Or_empty.t option =
        match t with
        | Non_empty range ->
          let module To_range = To_range (R) in
          To_range.to_range range
          |> Option.map ~f:(fun range -> R.Or_empty.Non_empty range)
        | Empty -> Some Empty
      ;;
    end
  end
end

module Time_of_measurement = struct
  type t =
    | Anywhere_in_range of Time_ns.Span.t Range_predicate.t
    | At of Time_ns.Span.t option
    | At_end_of_trace
  [@@deriving sexp, equal]
end

module String_predicate = struct
  type t =
    | Equals of string option
    | Contains of string option
  [@@deriving sexp, equal]

  let to_location_predicate = function
    | Equals (Some str) ->
      Some (Filter.Location_predicate.Defname_related { relation = Equals; rhs = str })
    | Contains (Some str) -> Some (Defname_related { relation = Contains; rhs = str })
    | Equals None | Contains None -> None
  ;;
end

module Which_heap = struct
  type t =
    | Minor
    | Major
  [@@deriving sexp, equal]
end

let time_span_range_of_range_predicate =
  let module T = Range_predicate.To_range (Range.Time_ns_span) in
  T.to_range
;;

let time_span_range_of_range_predicate_or_empty =
  let module T = Range_predicate.Or_empty.To_range (Range.Time_ns_span) in
  T.to_range
;;

let byte_units_range_of_range_predicate =
  let module T = Range_predicate.To_range (Range.Byte_units) in
  T.to_range
;;

module Byte_units_with_io : sig
  type t = Byte_units.t [@@deriving sexp, equal, compare, bin_io]
end = struct
  include Byte_units
  include Byte_units.Stable.V2
end

module Clause = struct
  type t =
    | Allocated of Time_ns.Span.t Range_predicate.t option
    | Live of Time_of_measurement.t option
    | Collected of Time_ns.Span.t Range_predicate.Or_empty.t option
    | Size of Byte_units_with_io.t Range_predicate.t option
    | Require_function of String_predicate.t option
    | Forbid_function of String_predicate.t option
    | Hide_function of String_predicate.t option
    | Heap of Which_heap.t option
  [@@deriving sexp, equal]

  let inter_opt ~inter range1_opt range2 =
    match range1_opt with
    | Some range1 -> inter range1 range2
    | None -> Some range2
  ;;

  let intersect_ranges ?allocated_range ?collected_range ?size_range (filter : Filter.t) =
    let open Option.Let_syntax in
    let%bind allocated_range =
      inter_opt ~inter:Range.Time_ns_span.inter_opt allocated_range filter.allocated_range
    in
    let%bind collected_range =
      let inter range1 range2 = Some (Range.Time_ns_span.Or_empty.inter range1 range2) in
      inter_opt ~inter collected_range filter.collected_range
    in
    let%map size_range =
      inter_opt ~inter:Range.Byte_units.inter_opt size_range filter.size_range
    in
    { filter with allocated_range; collected_range; size_range }
  ;;

  let make_bound_strict (bound : 'a Range.Bound.t) : 'a Range.Bound.t =
    match bound with
    | Closed a -> Open a
    | other -> other
  ;;

  let to_filter_modifier t (filter : Filter.t) =
    let open Option.Let_syntax in
    match t with
    | Allocated (Some pred) ->
      let%bind allocated_range = time_span_range_of_range_predicate pred in
      intersect_ranges ~allocated_range filter
    | Live (Some (At time)) ->
      let%bind time = time in
      (* This means it must be allocated at time <= t and collected at time > t *)
      let allocated_range = Range.Time_ns_span.range No_bound (Closed time) in
      let collected_range = Range.Time_ns_span.Or_empty.range (Open time) No_bound in
      intersect_ranges ~allocated_range ~collected_range filter
    | Live (Some At_end_of_trace) ->
      (* This means it's never collected *)
      Some { filter with collected_range = Empty }
    | Live (Some (Anywhere_in_range pred)) ->
      (* Live in range [t, t'] means allocated in range [0, t'] and collected in range
         (t, infty) *)
      let%bind range = time_span_range_of_range_predicate pred in
      let allocated_range = Range.Time_ns_span.range No_bound range.upper_bound in
      let collected_range =
        Range.Time_ns_span.Or_empty.range (make_bound_strict range.lower_bound) No_bound
      in
      intersect_ranges ~allocated_range ~collected_range filter
    | Collected (Some pred) ->
      let%bind collected_range = time_span_range_of_range_predicate_or_empty pred in
      intersect_ranges ~collected_range filter
    | Size (Some pred) ->
      let%bind size_range = byte_units_range_of_range_predicate pred in
      intersect_ranges ~size_range filter
    | Require_function (Some pred) ->
      let%map pred = pred |> String_predicate.to_location_predicate in
      { filter with required_locations = pred :: filter.required_locations }
    | Forbid_function (Some pred) ->
      let%map pred = pred |> String_predicate.to_location_predicate in
      { filter with forbidden_locations = pred :: filter.forbidden_locations }
    | Hide_function (Some pred) ->
      let%map pred = pred |> String_predicate.to_location_predicate in
      { filter with hidden_locations = pred :: filter.hidden_locations }
    | Heap (Some heap) ->
      (match heap with
       | Minor -> Some { filter with include_major_heap = false }
       | Major -> Some { filter with include_minor_heap = false })
    | Allocated None
    | Live None
    | Collected None
    | Size None
    | Require_function None
    | Forbid_function None
    | Hide_function None
    | Heap None -> None
  ;;
end

type t =
  { clauses : Clause.t option list
  ; direction : Filter.direction
  }
[@@deriving sexp, equal]

let to_filter_allow_incomplete { clauses; direction } =
  let clauses = List.filter_opt clauses in
  let filter =
    List.fold_left clauses ~init:Filter.default ~f:(fun filter clause ->
      Clause.to_filter_modifier clause filter |> Option.value ~default:filter)
  in
  { filter with direction }
;;

let to_filter { clauses; direction } =
  let open Option.Let_syntax in
  let%bind filter =
    List.fold_left clauses ~init:(Some Filter.default) ~f:(fun filter clause ->
      let%bind filter = filter
      and clause = clause in
      Clause.to_filter_modifier clause filter)
  in
  Option.some_if
    (filter.include_minor_heap || filter.include_major_heap)
    { filter with direction }
;;
