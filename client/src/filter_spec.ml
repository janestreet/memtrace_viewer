open! Core
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

  let to_range (type point) (module R : Range.S with type Point.t = point) (t : point t)
    : point Range.t option
    =
    match t with
    | At_least (Some lower_bound)
    | Between { lower_bound = Some lower_bound; upper_bound = None } ->
      Some (R.range (Closed lower_bound) No_bound)
    | At_most (Some upper_bound)
    | Between { lower_bound = None; upper_bound = Some upper_bound } ->
      Some (R.range No_bound (Closed upper_bound))
    | Between { lower_bound = Some lower_bound; upper_bound = Some upper_bound } ->
      Some (R.range (Closed lower_bound) (Closed upper_bound))
    | _ -> None
  ;;

  module Or_empty = struct
    type nonrec 'a t =
      | Non_empty of 'a t
      | Empty
    [@@deriving sexp, equal]

    let to_range
      (type point)
      (r : (module Range.S with type Point.t = point))
      (t : point t)
      : point Range.Or_empty.t option
      =
      match t with
      | Non_empty range ->
        to_range r range |> Option.map ~f:(fun range -> Range.Or_empty.Non_empty range)
      | Empty -> Some Empty
    ;;
  end
end

module Time_of_measurement = struct
  type t =
    | Anywhere_in_range of Time_ns.Span.t Range_predicate.t
    | At of Time_ns.Span.t option
    | At_end_of_trace
    | At_peak_allocations
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

module Area = struct
  type t =
    | Major
    | Minor
    | External
  [@@deriving sexp, equal]
end

module Area_predicate = struct
  type t =
    | Is of Area.t
    | Is_not of Area.t
  [@@deriving sexp, equal]
end

let time_span_range_of_range_predicate =
  Range_predicate.to_range (module Range.Time_ns_span)
;;

let time_span_range_of_range_predicate_or_empty =
  Range_predicate.Or_empty.to_range (module Range.Time_ns_span)
;;

let byte_units_range_of_range_predicate =
  Range_predicate.to_range (module Range.Byte_units)
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
    | Lifetime of Time_ns.Span.t Range_predicate.t option
    | Require_function of String_predicate.t option
    | Forbid_function of String_predicate.t option
    | Hide_function of String_predicate.t option
    | Area of Area_predicate.t option
  [@@deriving sexp, equal]

  let inter_opt ~inter range1_opt range2 =
    match range1_opt with
    | Some range1 -> inter range1 range2
    | None -> Some range2
  ;;

  let intersect_ranges
    ?allocated_range
    ?collected_range
    ?size_range
    ?lifetime_range
    (filter : Filter.t)
    =
    let open Option.Let_syntax in
    let%bind allocated_range =
      inter_opt ~inter:Range.Time_ns_span.inter_opt allocated_range filter.allocated_range
    in
    let%bind collected_range =
      let inter range1 range2 = Some (Range.Time_ns_span.Or_empty.inter range1 range2) in
      inter_opt ~inter collected_range filter.collected_range
    in
    let%bind size_range =
      inter_opt ~inter:Range.Byte_units.inter_opt size_range filter.size_range
    in
    let%map lifetime_range =
      inter_opt ~inter:Range.Time_ns_span.inter_opt lifetime_range filter.lifetime_range
    in
    { filter with allocated_range; collected_range; size_range; lifetime_range }
  ;;

  let live_at_time time filter =
    (* This means it must be allocated at time <= t and collected at time > t *)
    let allocated_range = Range.Time_ns_span.range No_bound (Closed time) in
    let collected_range = Range.Time_ns_span.Or_empty.range (Open time) No_bound in
    intersect_ranges ~allocated_range ~collected_range filter
  ;;

  let make_bound_strict (bound : 'a Range.Bound.t) : 'a Range.Bound.t =
    match bound with
    | Closed a -> Open a
    | other -> other
  ;;

  let add_area_predicate (pred : Area_predicate.t) (filter : Filter.t) =
    let no_major filter = { filter with Filter.include_major_heap = false } in
    let no_minor filter = { filter with Filter.include_minor_heap = false } in
    let no_external filter = { filter with Filter.include_external = false } in
    match pred with
    | Is_not Major -> no_major filter
    | Is_not Minor -> no_minor filter
    | Is_not External -> no_external filter
    | Is Major -> no_minor (no_external filter)
    | Is Minor -> no_major (no_external filter)
    | Is External -> no_minor (no_major filter)
  ;;

  let modify_filter t (filter : Filter.t) ~peak_allocations_time =
    let open Option.Let_syntax in
    match t with
    | Allocated (Some pred) ->
      let%bind allocated_range = time_span_range_of_range_predicate pred in
      intersect_ranges ~allocated_range filter
    | Live (Some (At time)) ->
      let%bind time in
      live_at_time time filter
    | Live (Some At_peak_allocations) -> live_at_time peak_allocations_time filter
    | Live (Some At_end_of_trace) ->
      (* This means it's never collected *)
      Some { filter with collected_range = Empty }
    | Live (Some (Anywhere_in_range pred)) ->
      (* Live in range [t, t'] means allocated in range [0, t'] and collected in range (t,
         infty) *)
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
    | Lifetime (Some pred) ->
      let%bind lifetime_range = time_span_range_of_range_predicate pred in
      intersect_ranges ~lifetime_range filter
    | Require_function (Some pred) ->
      let%map pred = pred |> String_predicate.to_location_predicate in
      { filter with required_locations = pred :: filter.required_locations }
    | Forbid_function (Some pred) ->
      let%map pred = pred |> String_predicate.to_location_predicate in
      { filter with forbidden_locations = pred :: filter.forbidden_locations }
    | Hide_function (Some pred) ->
      let%map pred = pred |> String_predicate.to_location_predicate in
      { filter with hidden_locations = pred :: filter.hidden_locations }
    | Area (Some pred) -> Some (add_area_predicate pred filter)
    | Allocated None
    | Live None
    | Collected None
    | Size None
    | Lifetime None
    | Require_function None
    | Forbid_function None
    | Hide_function None
    | Area None -> None
  ;;
end

type t = { clauses : Clause.t option list } [@@deriving sexp, equal]

let to_filter_allow_incomplete { clauses } ~peak_allocations_time =
  let clauses = List.filter_opt clauses in
  let filter =
    List.fold_left clauses ~init:Filter.always_true ~f:(fun filter clause ->
      Clause.modify_filter clause filter ~peak_allocations_time
      |> Option.value ~default:filter)
  in
  filter
;;

let valid_filter (filter : Filter.t) =
  filter.include_minor_heap || filter.include_major_heap || filter.include_external
;;

let to_filter { clauses } ~peak_allocations_time =
  let open Option.Let_syntax in
  let%bind filter =
    List.fold_left clauses ~init:(Some Filter.always_true) ~f:(fun filter clause ->
      let%bind filter and clause in
      Clause.modify_filter clause filter ~peak_allocations_time)
  in
  Option.some_if (valid_filter filter) filter
;;
