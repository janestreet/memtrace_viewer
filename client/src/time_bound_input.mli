open! Core
open Bonsai_web_proc

module Size : sig
  type t =
    | Small
    | Large
end

module Which_bound : sig
  type t =
    | Lower
    | Upper
end

type t = (Time_ns.Span.t option * Size.t) And_view.t

val component
  :  which:Which_bound.t
  -> max:Time_ns.Span.t Bonsai.Value.t
  -> start_time:Time_ns.t Bonsai.Value.t
  -> time_view:Graph_view.Time_view.t Bonsai.Value.t
  -> t Bonsai.Computation.t
