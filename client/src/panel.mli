open! Core
open Bonsai_web_proc

module State : sig
  type t =
    | Collapsed
    | Expanded
end

module Collapsible : sig
  type t =
    | No
    | Yes of { initial_state : State.t }
end

val panel
  :  ?title:string Value.t
  -> Vdom.Node.t Value.t
  -> id:string
  -> collapsible:Collapsible.t
  -> Vdom.Node.t Computation.t
