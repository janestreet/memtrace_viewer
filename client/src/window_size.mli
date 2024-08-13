open! Core
open! Bonsai_web.Proc

type t =
  { width : int
  ; height : int
  }

val component : t Computation.t
