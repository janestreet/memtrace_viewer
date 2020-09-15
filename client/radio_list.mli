open! Core_kernel
open Bonsai_web

module type Button = sig
  type t [@@deriving sexp, equal, enumerate]

  val label : t -> string
  val title : t -> string option
end

type 'a t

val view : _ t -> Vdom.Node.t
val value : 'a t -> 'a
val changing : _ t -> bool
val reset_changing : _ t -> Vdom.Event.t

val component
  :  (module Button with type t = 'a)
  -> name:string
  -> initial_value:'a
  -> 'a t Bonsai.Computation.t
