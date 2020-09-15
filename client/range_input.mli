open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

module Size : sig
  type t =
    | Small
    | Large
end

type t

val range : t -> Time_range.t
val changing : t -> bool
val lower_input : t -> Vdom.Node.t
val upper_input : t -> Vdom.Node.t
val reset_changing : t -> Vdom.Event.t
val size : t -> Size.t

val component
  :  initial_value:Time_range.t
  -> max:Time_ns.Span.t Bonsai.Value.t
  -> start_time:Time_ns.t Bonsai.Value.t
  -> time_view:Graph_view.Time_view.t Bonsai.Value.t
  -> t Bonsai.Computation.t
