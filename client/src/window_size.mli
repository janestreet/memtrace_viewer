open! Core
open! Bonsai_web

type t =
  { width : int
  ; height : int
  }

val component : t Computation.t
