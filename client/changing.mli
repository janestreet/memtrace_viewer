open! Core_kernel
open Bonsai_web

module type Value = sig
  type t [@@deriving sexp, equal]
end

type 'a t

val value : 'a t -> 'a
val set_value : 'a t -> 'a -> Vdom.Event.t
val changing : _ t -> bool
val set_changing : _ t -> bool -> Vdom.Event.t

val component
  :  (module Value with type t = 'a)
  -> initial_value:'a
  -> 'a t Bonsai.Computation.t
