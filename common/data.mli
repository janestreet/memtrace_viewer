open! Core_kernel

module Location : sig
  type t [@@deriving sexp, bin_io, compare]

  val filename : t -> string
  val line : t -> int
  val start_char : t -> int
  val end_char : t -> int
  val defname : t -> string

  val create
    :  filename:string
    -> line:int
    -> start_char:int
    -> end_char:int
    -> defname:string
    -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Backtrace : sig
  type t = Location.t list [@@deriving sexp, bin_io, compare]

  include Comparable.S with type t := t

  module Reversed : sig
    type backtrace := t
    type t [@@deriving sexp, compare]

    val of_forward : backtrace -> t
    val of_reversed_list : Location.t list -> t
    val elements : t -> Location.t list
    val nil : t
    val cons : Location.t -> t -> t

    include Comparable.S with type t := t
  end

  val of_reversed : Reversed.t -> t
end

module Graph : sig
  (* (time, major heap size) pairs *)
  type t [@@deriving sexp, bin_io]

  val create : (Time_ns.Span.t * Byte_units.t) list -> t
  val points : t -> (Time_ns.Span.t * Byte_units.t) list
  val max_x : t -> Time_ns.Span.t
  val max_y : t -> Byte_units.t
end

module Entry : sig
  module On_unroll : sig
    type t =
      | Keep
      (** This node has enough allocations on its own to be significant even when unrolled
      *)
      | Hide (** Only this node's children are significant, so hide it when unrolled *)
    [@@deriving sexp, bin_io, equal]
  end

  type t [@@deriving sexp, bin_io, equal]

  val allocations : t -> Byte_units.t
  val allocations_excluding_children : t -> Byte_units.t
  val on_unroll : t -> On_unroll.t

  val create
    :  allocations:Byte_units.t
    -> allocations_excluding_children:Byte_units.t
    -> on_unroll:On_unroll.t
    -> t
end

module Trie : sig
  module Node : sig
    type t [@@deriving sexp, bin_io]

    val entry : t -> Entry.t
    val children : t -> t Location.Map.t
    val create : entry:Entry.t -> children:t Location.Map.t -> t
  end

  type t [@@deriving sexp, bin_io]

  val roots : t -> Node.t Location.Map.t
  val total_allocations : t -> Byte_units.t
  val create : roots:Node.t Location.Map.t -> total_allocations:Byte_units.t -> t
  val find : t -> Backtrace.t -> Node.t option

  val fold
    :  t
    -> init:'a
    -> f:(backtrace_rev:Backtrace.Reversed.t -> entry:Entry.t -> 'a -> 'a)
    -> 'a
end

module Info : sig
  type t =
    { sample_rate : float
    ; word_size : int
    ; executable_name : string
    ; host_name : string
    ; ocaml_runtime_params : string
    ; pid : Int64.t
    ; start_time : Time_ns.t
    ; context : string option
    }
  [@@deriving sexp, bin_io]
end

type t =
  { graph : Graph.t
  ; filtered_graph : Graph.t option (* could also merge the two graphs *)
  ; trie : Trie.t
  ; direction : Filter.direction
  ; total_allocations_unfiltered : Byte_units.t
  ; info : Info.t option
  }
[@@deriving sexp, bin_io]

val empty : t
