open! Core

module Status : sig
  type t =
    | Idle
    | Busy
    | Down

  val is_idle : t -> bool
end

type t = { status : Status.t }

val initial : t
