open! Core

module type Location = sig
  type t [@@deriving bin_io, sexp, equal]

  include Hashable.S with type t := t

  val dummy : t

  module Debug : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module type Entry = sig
  type t [@@deriving bin_io, sexp]

  module Debug : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module type Metadata = sig
  type t [@@deriving bin_io, sexp]
end

module type Suffix_tree = sig
  type location
  type entry

  module Node : sig
    module Id : sig
      type t

      include Hashable.S with type t := t
      include Sexpable.S with type t := t
    end

    type t

    val id : t -> Id.t
    val entry : t -> entry
    val incoming_edge : t -> location
    val suffix : t -> t option
    val children : t -> (location, t) List.Assoc.t
    val representative : t -> t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  type t

  val root : t -> Node.t
end

module type Backtrace = sig
  module Location : Location

  (** A backtrace, represented from toplevel to allocation site *)
  type t = Location.t list [@@deriving sexp, bin_io, compare, hash]

  include Comparable.S with type t := t

  module Debug : sig
    type nonrec t = t [@@deriving sexp_of]
  end

  module Reversed : sig
    type backtrace := t

    (** A backtrace, represented from allocation site to toplevel *)
    type t [@@deriving sexp, bin_io, compare, hash]

    val of_forward : backtrace -> t
    val of_reversed_list : Location.t list -> t
    val elements : t -> Location.t list
    val nil : t
    val cons : Location.t -> t -> t
    val append : t -> t -> t
    val hd : t -> Location.t option
    val tl : t -> t option
    val head_and_tail : t -> (Location.t * t) option

    include Comparable.S with type t := t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  val of_reversed : Reversed.t -> t
end

module type Fragment = sig
  module Location : Location
  module Entry : Entry
  module Backtrace : Backtrace with module Location := Location

  module Id : sig
    type t [@@deriving bin_io]

    include Hashable.S with type t := t
    include Sexpable.S with type t := t
    include Comparable.S with type t := t
  end

  type t

  val id : t -> Id.t
  val is_empty : t -> bool
  val is_singleton : t -> bool
  val same : t -> t -> bool
  val entry : t -> Entry.t
  val first : t -> orient:Orientation.t -> Location.t
  val backtrace : t -> Backtrace.t
  val backtrace_rev : t -> Backtrace.Reversed.t
  val retract : t -> orient:Orientation.t -> t option
  val retract_by : t -> orient:Orientation.t -> n:int -> t option
  val one_frame_extensions : t -> orient:Orientation.t -> (Location.t, t) List.Assoc.t
  val has_extensions : t -> orient:Orientation.t -> bool
  val extend : t -> orient:Orientation.t -> Location.t -> t option
  val extend_by_callers : t -> Backtrace.Reversed.t -> t option
  val extend_by_callees : t -> Backtrace.t -> t option
  val is_extension : t -> extension:t -> orient:Orientation.t -> bool
  val representative : t -> t
  val length : t -> int

  module Debug : sig
    type nonrec t = t [@@deriving sexp_of]
  end

  module Oriented : sig
    type fragment := t

    (** A fragment and an orientation. Can be thought of as a tree node -- with its
        extensions as children and it's retraction as parent. *)
    type t

    val fragment : t -> fragment
    val orient : t -> Orientation.t
    val first : t -> Location.t
    val retract : t -> t option
    val retract_by : t -> n:int -> t option
    val one_frame_extensions : t -> (Location.t, t) List.Assoc.t
    val has_extensions : t -> bool
    val extend : t -> Location.t -> t option

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  val oriented : t -> orient:Orientation.t -> Oriented.t

  module Iterator : sig
    type fragment := t

    (** An iterator that iterates through the prefixes of a fragment from callers to
        callees. *)
    type t

    val next : t -> t option
    val prev : t -> t option
    val location : t -> Location.t
    val prefix : t -> fragment
    val suffix : t -> fragment

    module Trace : sig
      type t = private
        { prefix_trace : Backtrace.Reversed.t
        ; suffix_trace : Backtrace.t
        }
      [@@deriving sexp, bin_io, compare]

      include Comparable.S with type t := t
    end

    val trace : t -> Trace.t
  end

  val iterator_start : t -> Iterator.t option
  val iterator_end : t -> Iterator.t option
end

module type Trie = sig
  module Location : Location
  module Entry : Entry
  module Metadata : Metadata
  module Backtrace : Backtrace with module Location := Location

  module type Suffix_tree =
    Suffix_tree with type location := Location.t and type entry := Entry.t

  module Fragment :
    Fragment
    with module Location := Location
     and module Entry := Entry
     and module Backtrace := Backtrace

  type t [@@deriving sexp]

  val of_suffix_tree
    :  (module Suffix_tree with type t = 'a)
    -> 'a
    -> metadata:Metadata.t
    -> t

  val metadata : t -> Metadata.t
  val empty_fragment : t -> Fragment.t
  val find : t -> Backtrace.t -> Fragment.t option
  val find_rev : t -> Backtrace.Reversed.t -> Fragment.t option
  val find_singleton : t -> Location.t -> Fragment.t option
  val find_iterator : t -> Fragment.Iterator.Trace.t -> Fragment.Iterator.t option

  val deep_fold_callers
    :  t
    -> init:'a
    -> f:(backtrace:Backtrace.t -> fragment:Fragment.t -> 'a -> 'a)
    -> 'a

  val deep_fold_callees
    :  t
    -> init:'a
    -> f:(backtrace_rev:Backtrace.Reversed.t -> fragment:Fragment.t -> 'a -> 'a)
    -> 'a

  val fold_singletons
    :  t
    -> init:'a
    -> f:(location:Location.t -> fragment:Fragment.t -> 'a -> 'a)
    -> 'a

  module Serialized : sig
    type unserialized := t
    type t [@@deriving sexp, bin_io]

    val serialize : unserialized -> t
    val unserialize : t -> unserialized
  end

  module Debug : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module type S = sig
  module Location : Location
  module Entry : Entry
  module Metadata : Metadata

  module type Suffix_tree =
    Suffix_tree with type location := Location.t and type entry := Entry.t

  module Backtrace : Backtrace with module Location := Location

  module Fragment :
    Fragment
    with module Location := Location
     and module Entry := Entry
     and module Backtrace := Backtrace

  module Trie :
    Trie
    with module Location := Location
     and module Entry := Entry
     and module Metadata := Metadata
     and module Backtrace := Backtrace
     and module Fragment := Fragment

  module For_testing : sig
    module Dumped : sig
      type t = Trie.t [@@deriving sexp_of]
    end
  end
end

module type Fragment_trie = sig
  module type Location = Location
  module type Entry = Entry
  module type Metadata = Metadata
  module type Backtrace = Backtrace
  module type Suffix_tree = Suffix_tree
  module type Fragment = Fragment
  module type Trie = Trie
  module type S = S

  module Make (Location : Location) (Entry : Entry) (Metadata : Metadata) :
    S
    with module Location := Location
     and module Entry := Entry
     and module Metadata := Metadata
end
