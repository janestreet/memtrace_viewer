open! Core
open Bonsai_web.Proc

type t = Byte_units.t option And_view.t

val component : t Bonsai.Computation.t
