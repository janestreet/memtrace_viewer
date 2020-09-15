open! Core_kernel

module Make (X : Hashable.S_plain) : sig
  type t

  val create : float -> t
  val insert : t -> X.t list -> int -> unit
  val output : t -> float -> (X.t list * int * int) list
  val total : t -> int

  module Node : sig
    type t

    val children : t -> (X.t * t) list
    val samples_excluding_children : t -> int
    val delta : t -> int
  end

  val roots : t -> (X.t * Node.t) list
end
