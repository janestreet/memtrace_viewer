open! Core_kernel

module Status : sig
  type t =
    | Up
    | Down

  val is_up : t -> bool
end

type t = { status : Status.t }

val initial : t
