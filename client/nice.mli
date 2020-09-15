open! Core_kernel

(** Round the given number to the nearest number of the form [n *. 10. ** k], where [k] is
    an integer and [n] is 1, 2, or 5. *)
val round : float -> float

(** Round the given number down to the next number of the form [n *. 10. ** k], where [k]
    is an integer and [n] is 1, 2, or 5. *)
val round_down : float -> float

(** Round the given number up to the next number of the form [n *. 10. ** k], where [k] is
    an integer and [n] is 1, 2, or 5. *)
val round_up : float -> float

(** Generate labels for an axis of a graph, given the range of values on that axis. Labels
    will be evenly spaced with a "nice" gap between them, where a nice number is [n *
    10**k] where n is 1, 2, or 5, and all labels will be within the given range. (This is
    different from the usual "loose labels" algorithm, found for instance in "Nice Numbers
    for Graph Labels" in Graphics Gems vol. 1, which places the labels outside the range
    of the data. We want our time series to cover the whole x-axis, so the last label
    won't reach quite to the edge.) *)
val loose_labels : ?max_count:int -> float -> float -> float list

module Time_ns : sig
  val round_down_to_multiple_of_nice
    :  relative_to:Time_ns.t
    -> step:Time_ns.Span.t
    -> Time_ns.t
    -> Time_ns.t

  val round_up_to_multiple_of_nice
    :  relative_to:Time_ns.t
    -> step:Time_ns.Span.t
    -> Time_ns.t
    -> Time_ns.t

  val loose_labels
    :  ?max_count:int
    -> relative_to:Time_ns.t
    -> Time_ns.t
    -> Time_ns.t
    -> Time_ns.t list

  val start_of_day_utc : Time_ns.t -> Time_ns.t

  module Span : sig
    val round : Time_ns.Span.t -> Time_ns.Span.t
    val round_down : Time_ns.Span.t -> Time_ns.Span.t
    val round_up : Time_ns.Span.t -> Time_ns.Span.t

    val loose_labels
      :  ?max_count:int
      -> Time_ns.Span.t
      -> Time_ns.Span.t
      -> Time_ns.Span.t list
  end
end
