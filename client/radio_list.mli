open! Core
open Bonsai_web

module type Button = sig
  type t [@@deriving sexp, equal, enumerate]

  val label : t -> string
  val title : t -> string option
end

val component
  :  (module Button with type t = 'button)
  -> name:string Bonsai.Value.t
  -> value:'button option Bonsai.Value.t
  -> set_value:('button -> unit Vdom.Effect.t) Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
