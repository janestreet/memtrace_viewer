open! Core

module Status : sig
  type t =
    | Up
    | Down

  val is_up : t -> bool
end

type t = { status : Status.t }

val initial : t
