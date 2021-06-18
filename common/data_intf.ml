open! Core

module type Suffix_tree = sig
  type entry
  type location

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
  val total_allocations : t -> Byte_units.t
  val significance_threshold : t -> Byte_units.t
end

module type Data = sig
  module Location : sig
    type t [@@deriving sexp, bin_io]

    val defname : t -> string
    val full_name : t -> string
    val loc_in_file : t -> string

    val create
      :  filename:string
      -> line:int
      -> start_char:int
      -> end_char:int
      -> defname:string
      -> t

    val allocation_site : t
    val toplevel : t
    val dummy : t
    val is_allocation_site : t -> bool
    val is_toplevel : t -> bool
    val is_dummy : t -> bool
    val is_special : t -> bool

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  module Backtrace : sig
    (** A backtrace, represented from toplevel to allocation site **)
    type t = Location.t list [@@deriving sexp, bin_io, compare]

    include Comparable.S with type t := t

    (** Returns true if the backtrace is empty or a singleton of a special location *)
    val is_trivial : t -> bool

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    module Reversed : sig
      type backtrace := t

      (** A backtrace, represented from allocation site to toplevel *)
      type t [@@deriving sexp]

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

  module Graph : sig
    (* (time, major heap size) pairs *)
    type t [@@deriving sexp, bin_io]

    val create : (Time_ns.Span.t * Byte_units.t) list -> t
    val points : t -> (Time_ns.Span.t * Byte_units.t) list
    val max_x : t -> Time_ns.Span.t
    val max_y : t -> Byte_units.t
  end

  module Entry : sig
    type t [@@deriving sexp, bin_io]

    val allocations : t -> Byte_units.t
    val direct_allocations : t -> Byte_units.t
    val is_heavy : t -> bool
    val allocations_string : t -> string
    val percentage_string : t -> string

    val create
      :  total_allocations_in_trie:Byte_units.t
      -> allocations:Byte_units.t
      -> direct_allocations:Byte_units.t
      -> is_heavy:bool
      -> t
  end

  module Orientation : sig
    type t =
      | Callers
      |
        Callees
    [@@deriving sexp, equal]

    val flip : t -> t
  end

  module Fragment : sig
    module Id : sig
      type t

      include Hashable.S with type t := t
      include Sexpable.S with type t := t
      include Comparable.S with type t := t
    end

    type t

    val id : t -> Id.t
    val is_empty : t -> bool
    val same : t -> t -> bool
    val entry : t -> Entry.t
    val first : t -> orient:Orientation.t -> Location.t
    val backtrace : t -> Backtrace.t
    val backtrace_rev : t -> Backtrace.Reversed.t
    val retract : t -> orient:Orientation.t -> t option
    val retract_by : t -> orient:Orientation.t -> n:int -> t option
    val one_frame_extensions : t -> orient:Orientation.t -> (Location.t, t) List.Assoc.t
    val has_extensions : t -> orient:Orientation.t -> bool
    val is_trivial : t -> bool
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

      (** A fragment and an orientation. Can be thought of as a tree node --
          with it's extensions as children and it's retraction as parent. *)
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

      (** An iterator that iterates through the prefixes of a fragment
          from callers to callees. *)
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

  module Fragment_trie : sig
    type t [@@deriving sexp, bin_io]

    module type Suffix_tree =
      Suffix_tree with type entry := Entry.t and type location := Location.t

    val empty_fragment : t -> Fragment.t
    val allocation_site_fragment : t -> Fragment.t
    val toplevel_fragment : t -> Fragment.t
    val total_allocations : t -> Byte_units.t
    val of_suffix_tree : (module Suffix_tree with type t = 'a) -> 'a -> t
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
  end

  module type Suffix_tree = Fragment_trie.Suffix_tree

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
    ; trie : Fragment_trie.t
    ; total_allocations_unfiltered : Byte_units.t
    ; hot_paths : Fragment.t list
    ; hot_call_sites : Fragment.t list
    ; info : Info.t option
    }
  [@@deriving sexp, bin_io]

  val empty : t
end
