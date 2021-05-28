open! Core_kernel

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

    val allocation_site : t
    val toplevel : t
    val dummy : t
    val is_allocation_site : t -> bool
    val is_toplevel : t -> bool
    val is_dummy : t -> bool
    val is_special : t -> bool
    val to_string : t -> string
    val loc_in_file_to_string : t -> string

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
    val max_error : t -> Byte_units.t
    val create : allocations:Byte_units.t -> max_error:Byte_units.t -> t
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
    end

    type t

    val id : t -> Id.t
    val is_empty : t -> bool
    val same : t -> t -> bool
    val entry : t -> Entry.t
    val first : t -> orient:Orientation.t -> Location.t
    val last : t -> orient:Orientation.t -> Location.t
    val backtrace : t -> Backtrace.t
    val backtrace_rev : t -> Backtrace.Reversed.t
    val retract : t -> orient:Orientation.t -> t option
    val one_frame_extensions : t -> orient:Orientation.t -> (Location.t, t) List.Assoc.t
    val all_one_frame_extensions : t -> t list
    val extend_by_callers : t -> Backtrace.Reversed.t -> t option
    val extend_by_callees : t -> Backtrace.t -> t option
    val extend_by : t -> Backtrace.t -> orient:Orientation.t -> t option
    val is_extension : t -> extension:t -> strictly:bool -> orient:Orientation.t -> bool
    val is_one_sided_extension : t -> extension:t -> strictly:bool -> bool
    val representative : t -> t

    module Extension : sig
      type t =
        { callers : Backtrace.Reversed.t
        ; callees : Backtrace.t
        }
      [@@deriving sexp]

      include Comparable.S with type t := t

      val empty : t
      val of_callers : Backtrace.Reversed.t -> t
      val of_callees : Backtrace.t -> t

      module Debug : sig
        type nonrec t = t [@@deriving sexp_of]
      end
    end

    val extend : t -> Extension.t -> t option


    (** Given [t] that's a one-sided sub-backtrace of [extension], get the fragment with the
        same end frame as [t] that is a sub-backtrace of [extension] on the other side.

        {v
          |------------ extension -----------|
          .    .    . end .   .    .    .    .
          |------ t ------|
                    |-------- flip t --------|
       v}

        The motivating case is where t represents a frame that is focused, so it's /really/
        the end that's the focused frame, and we need to find the representation of that
        frame in the other direction (that is, in the other flame graph).

        Precondition: [is_extension t ~extension ~orient ~strictly:false]
    *)
    val flip : t -> extension:t -> orient:Orientation.t -> t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    module Oriented : sig

      (** A fragment as understood to stand for either a backtrace and its callers or a
          backtrace and its callees. In practical terms, this means whether it appears in
          the flame graph (callees) or the icicle graph (callers).

          Note that this isn't usually an actual degree of freedom, since each node on the
          flame graph (besides the zoom nodes) represents a fragment that extends the zoom
          fragment on one side or the other, and that side determines which graph the node
          must be on. Nonetheless it's convenient to tag it. *)
      type nonrec t =
        { fragment : t
        ; orient : Orientation.t
        }

      val first : t -> Location.t
      val retract : t -> t option
      val one_frame_extensions : t -> (Location.t, t) List.Assoc.t

      module Debug : sig
        type nonrec t = t [@@deriving sexp_of]
      end
    end
  end

  module Fragment_trie : sig
    type t [@@deriving sexp, bin_io]

    module type Suffix_tree =
      Suffix_tree with type entry := Entry.t and type location := Location.t

    val empty_fragment : t -> Fragment.t
    val singleton_fragments : t -> Fragment.t Location.Table.t
    val allocation_site_fragment : t -> Fragment.t
    val toplevel_fragment : t -> Fragment.t
    val total_allocations : t -> Byte_units.t
    val significance_threshold : t -> Byte_units.t
    val of_suffix_tree : (module Suffix_tree with type t = 'a) -> 'a -> t
    val find : t -> Backtrace.t -> Fragment.t option
    val find_rev : t -> Backtrace.Reversed.t -> Fragment.t option

    val deep_fold_callers
      :  t
      -> init:'a
      -> f:(backtrace:Backtrace.t -> entry:Entry.t -> 'a -> 'a)
      -> 'a

    val deep_fold_callees
      :  t
      -> init:'a
      -> f:(backtrace_rev:Backtrace.Reversed.t -> entry:Entry.t -> 'a -> 'a)
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
    ; info : Info.t option
    }
  [@@deriving sexp, bin_io]

  val empty : t
end
