open! Core_kernel
open Bonsai_web

module type Button = sig
  type t [@@deriving sexp, equal, enumerate]

  val label : t -> string
  val title : t -> string option
end

type 'a t = 'a And_view.t

val component
  :  (module Button with type t = 'a)
  -> name:string
  -> initial_value:'a
  -> 'a t Bonsai.Computation.t
