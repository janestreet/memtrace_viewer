open! Core_kernel
open! Bonsai_web

module Series : sig
  type t

  val create
    :  ?css_class:string
    -> max_x:Time_ns.Span.t
    -> max_y:float
    -> (Time_ns.Span.t * float) list
    -> t
end

module Region : sig
  type t

  val create : ?css_class:string -> Time_ns.Span.t option -> Time_ns.Span.t option -> t
end

module Time_view : sig
  type t =
    | Elapsed_seconds
    | Wall_time
  [@@deriving sexp, equal]
end

val component
  :  series:Series.t list Bonsai.Value.t
  -> regions:Region.t list Bonsai.Value.t
  -> width:int Bonsai.Value.t
  -> height:int Bonsai.Value.t
  -> start_time:Time_ns.t Bonsai.Value.t
  -> time_view:Time_view.t Bonsai.Value.t
  -> set_time_view:(Time_view.t -> Vdom.Event.t) Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
