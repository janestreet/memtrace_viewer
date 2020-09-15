open! Core_kernel
open Bonsai_web

module type Tab = sig
  type t [@@deriving enumerate, sexp, compare, equal]

  module Input : T
  module Result : T

  val name : t -> string
  val initial : t

  val component
    :  t
    -> input:Input.t Bonsai.Value.t
    -> select_tab:(t -> Vdom.Event.t) Bonsai.Value.t
    -> (Vdom.Node.t * Result.t) Bonsai.Computation.t
end

module type S = sig
  module type Tab = Tab

  val component
    :  (module Tab with type Input.t = 'input and type Result.t = 'result)
    -> 'input Bonsai.Value.t
    -> (Vdom.Node.t * 'result) Bonsai.Computation.t
end
