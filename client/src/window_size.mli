open! Core
open! Bonsai_web_proc

type t =
  { width : int
  ; height : int
  }

val component : t Computation.t
