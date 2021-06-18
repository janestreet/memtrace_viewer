open! Core
open Memtrace_viewer_common

module type Char = sig
  include Hashable.S_plain
  include Comparable.S_plain with type t := t
  include Sexpable.S with type t := t

  (** Must be the greatest element according to the [Comparable] ordering *)
  val dummy : t
end

module Make (X : Char) : sig
  type t

  module Node : sig
    module Id : Identifier.S

    type t

    val id : t -> Id.t
    val label : t -> X.t array

    module Root : sig
      type node := t
      type t

      val node : t -> node
    end

    val get_child : root:Root.t -> t -> X.t -> t
    val edge_length : t -> int
    val edge_char : t -> int -> X.t
    val iter_children : t -> root:Root.t -> f:(t -> unit) -> unit
    val fold_children : t -> root:Root.t -> init:'a -> f:(t -> 'a -> 'a) -> 'a
    val has_suffix : t -> bool
    val suffix : t -> t
    val parent : t -> t
    val representative : t -> t
    val total_count : t -> int
    val light_count : t -> int

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    module Debug_full : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  val create : tolerance:float -> t
  val insert : t -> common_prefix:int -> X.t array -> count:int -> unit
  val total_count : t -> int
  val maximum_depth : t -> int
  val calculate_totals : t -> heaviness_frequency:float -> unit
  val root : t -> Node.Root.t
  val is_heavy : t -> Node.t -> bool
  val contains_heavy : t -> Node.t -> bool
end
