open! Core
open Memtrace_viewer_common

module Range_predicate : sig
  type 'a t =
    | At_least of 'a option
    | At_most of 'a option
    | Between of
        { lower_bound : 'a option
        ; upper_bound : 'a option
        }
  [@@deriving sexp, equal]

  module Or_empty : sig
    type nonrec 'a t =
      | Non_empty of 'a t
      | Empty
    [@@deriving sexp, equal]
  end
end

(* A range predicate constrains the time at which an event (allocation or collection)
   occurs. A time of measurement instead specifies a time or range of times at which an
   object's state (liveness) is measured. *)
module Time_of_measurement : sig
  type t =
    | Anywhere_in_range of Time_ns.Span.t Range_predicate.t
    | At of Time_ns.Span.t option
    | At_end_of_trace
    | At_peak_allocations
  [@@deriving sexp, equal]
end

module String_predicate : sig
  type t =
    | Equals of string option
    | Contains of string option
  [@@deriving sexp, equal]
end

module Area : sig
  type t =
    | Major
    | Minor
    | External
  [@@deriving sexp, equal]
end

module Area_predicate : sig
  type t =
    | Is of Area.t
    | Is_not of Area.t
  [@@deriving sexp, equal]
end

module Clause : sig
  type t =
    | Allocated of Time_ns.Span.t Range_predicate.t option
    | Live of Time_of_measurement.t option
    | Collected of Time_ns.Span.t Range_predicate.Or_empty.t option
    | Size of Byte_units.t Range_predicate.t option
    | Lifetime of Time_ns.Span.t Range_predicate.t option
    | Require_function of String_predicate.t option
    | Forbid_function of String_predicate.t option
    | Hide_function of String_predicate.t option
    | Area of Area_predicate.t option
  [@@deriving sexp, equal]
end

type t = { clauses : Clause.t option list } [@@deriving sexp, equal]

val to_filter : t -> peak_allocations_time:Time_ns.Span.t -> Filter.t option
val to_filter_allow_incomplete : t -> peak_allocations_time:Time_ns.Span.t -> Filter.t
