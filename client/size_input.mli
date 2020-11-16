open! Core_kernel
open Bonsai_web

type t = Byte_units.t option And_view.t

val component : t Bonsai.Computation.t
