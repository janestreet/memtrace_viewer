open! Core_kernel
open Bonsai_web

type t

val view : t -> Vdom.Node.t
val value : t -> bool
val changing : t -> bool
val reset_changing : t -> Vdom.Event.t
val component : label:string -> initial_value:bool -> t Bonsai.Computation.t
