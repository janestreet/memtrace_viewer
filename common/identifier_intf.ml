open! Core

module type S = sig
  type t = private int [@@deriving equal, compare, sexp, bin_io]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  module Generator : sig
    type id := t
    type t [@@deriving sexp]

    val create : unit -> t
    val generate : t -> id
  end
end

module type S_with_special = sig
  include S

  val max_value : t
  val first_special : t
  val next_special : t -> t
  val is_special : t -> bool
end

module type Identifier = sig
  module type S = S
  module type S_with_special = S_with_special

  module Make () : S_with_special
end
