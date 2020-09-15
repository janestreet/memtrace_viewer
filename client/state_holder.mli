open! Core_kernel
open Bonsai_web

type 'a t =
  { current : 'a
  ; set : 'a -> Vdom.Event.t
  }

val component
  :  (module Bonsai_web.Bonsai.Model with type t = 'a)
  -> initial:'a
  -> 'a t Bonsai.Computation.t
