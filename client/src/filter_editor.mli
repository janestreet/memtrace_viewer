open! Core
open Bonsai_web_proc

type t = Filter_spec.Clause.t option list And_view.t

val component
  :  max_time:Time_ns.Span.t Bonsai.Value.t
  -> start_time:Time_ns.t Bonsai.Value.t
  -> time_view:Graph_view.Time_view.t Bonsai.Value.t
  -> t Bonsai.Computation.t
