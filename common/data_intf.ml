open! Core

module type Data = sig
  module Call_site : sig
    type t [@@deriving sexp, bin_io]

    val defname : t -> string
    val filename : t -> string
    val position : t -> string

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

  module Allocation_site : sig
    type t [@@deriving sexp, bin_io]

    val of_call_site : Call_site.t -> t
    val defname : t -> string
    val filename : t -> string
    val position : t -> string
    val short_name : t -> string
    val full_name : t -> string

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  module Function : sig
    type t [@@deriving sexp, bin_io]

    val defname : t -> string
    val full_name : t -> call_sites:Call_site.t list option -> string
    val create : defname:string -> t

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  module Location : sig
    type t = private
      | Function of Function.t
      | Allocation_site of Allocation_site.t
      | Allocator
      | Toplevel
      | Dummy
    [@@deriving sexp, bin_io]

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val defname : t -> string
    val full_name : t -> call_sites:Call_site.t list option -> string
    val create_function : Function.t -> t
    val create_allocation_site : Call_site.t -> t
    val allocator : t
    val toplevel : t
    val dummy : t
    val is_allocator : t -> bool
    val is_toplevel : t -> bool
    val is_dummy : t -> bool
    val is_special : t -> bool

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  module Call_sites : sig
    type t [@@deriving sexp]

    val create : (Function.t * Call_site.t list) list -> t
    val for_function : t -> Function.t -> Call_site.t list
    val for_location : t -> Location.t -> Call_site.t list
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

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  module Metadata : Fragment_trie.Metadata

  module Backtrace : sig
    include Fragment_trie.Backtrace with module Location := Location

    val is_trivial : t -> bool
  end

  module Fragment : sig
    include
      Fragment_trie.Fragment
      with module Location := Location
       and module Entry := Entry
       and module Backtrace := Backtrace

    val is_trivial : t -> bool
  end

  module type Suffix_tree =
    Fragment_trie.Suffix_tree with type location := Location.t and type entry := Entry.t

  module Fragment_trie : sig
    include
      Fragment_trie.Trie
      with module Location := Location
       and module Entry := Entry
       and module Metadata := Metadata
       and module Backtrace := Backtrace
       and module Fragment := Fragment

    val of_suffix_tree
      :  (module Suffix_tree with type t = 'tree)
      -> 'tree
      -> total_allocations:Byte_units.t
      -> t

    val allocator_fragment : t -> Fragment.t
    val toplevel_fragment : t -> Fragment.t
    val total_allocations : t -> Byte_units.t
  end

  module Info : sig
    type t =
      { sample_rate : float
      ; word_size : Byte_units.t
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
    ; peak_allocations : Byte_units.t
    ; peak_allocations_time : Time_ns.Span.t
    ; call_sites : Call_sites.t
    ; hot_paths : Fragment.t list
    ; hot_locations : Fragment.t list
    ; info : Info.t option
    }
  [@@deriving sexp]

  val empty : t

  module Serialized : sig
    type unserialized := t
    type t [@@deriving sexp, bin_io]

    val serialize : unserialized -> t
    val unserialize : t -> unserialized
  end
end
