open! Core_kernel

module Location = struct
  module T = struct
    type t =
      { filename : string
      ; line : int
      ; start_char : int
      ; end_char : int
      ; defname : string
      }
    [@@deriving sexp, bin_io, compare, hash, fields]
  end

  include T
  include Comparable.Make_binable (T)
  include Hashable.Make (T)

  let create ~filename ~line ~start_char ~end_char ~defname =
    { filename; line; start_char; end_char; defname }
  ;;
end

module Backtrace = struct
  module T = struct
    type t = Location.t list [@@deriving sexp, bin_io, compare]
  end

  include T
  include Comparable.Make_binable (T)

  module Reversed = struct
    module T = T
    include T
    include Comparable.Make_binable (T)

    let nil = []
    let cons loc t = loc :: t
    let of_forward t = List.rev t
    let of_reversed_list t = t
    let elements t = t
  end

  let of_reversed (t : Reversed.t) = List.rev t
end

module Graph = struct
  type t =
    { points : (Time_ns.Span.t * Byte_units.Stable.V2.t) list
    ; max_x : Time_ns.Span.t
    ; max_y : Byte_units.Stable.V2.t
    }
  [@@deriving sexp, bin_io, fields]

  let create points =
    let max_x, max_y =
      List.fold_left
        points
        ~init:(Time_ns.Span.zero, Byte_units.zero)
        ~f:(fun (_, max_y) (x, y) -> x, Byte_units.max max_y y)
    in
    { points; max_x; max_y }
  ;;
end

module Byte_units_binable_and_equal : sig
  type t = Byte_units.t [@@deriving sexp, bin_io, equal]
end = struct
  include Byte_units
  include Byte_units.Stable.V2
end

module Entry = struct
  module On_unroll = struct
    type t =
      | Keep
      | Hide
    [@@deriving sexp, bin_io, equal]
  end

  type t =
    { allocations : Byte_units_binable_and_equal.t
    ; allocations_excluding_children : Byte_units_binable_and_equal.t
    ; on_unroll : On_unroll.t
    }
  [@@deriving sexp, bin_io, fields, equal]

  let create ~allocations ~allocations_excluding_children ~on_unroll =
    { allocations; allocations_excluding_children; on_unroll }
  ;;
end

module Trie = struct
  module Node = struct
    type t =
      { entry : Entry.t
      ; children : t Location.Map.t
      }
    [@@deriving sexp, bin_io, fields]

    let create ~entry ~children = { entry; children }

    let rec fold t ~backtrace_rev ~init ~f =
      let init = f ~backtrace_rev ~entry:t.entry init in
      Location.Map.fold t.children ~init ~f:(fun ~key:loc ~data:entry a ->
        let backtrace_rev = loc :: backtrace_rev in
        fold ~backtrace_rev ~init:a ~f entry)
    ;;

    let rec find t backtrace =
      match backtrace with
      | [] -> Some t
      | loc :: locs ->
        let%bind.Option child = Location.Map.find t.children loc in
        find child locs
    ;;
  end

  type t =
    { roots : Node.t Location.Map.t
    ; total_allocations : Byte_units.Stable.V2.t
    }
  [@@deriving sexp, bin_io, fields]

  let create ~roots ~total_allocations = { roots; total_allocations }
  let empty = { roots = Location.Map.empty; total_allocations = Byte_units.zero }

  let fold t ~init ~f =
    Location.Map.fold t.roots ~init ~f:(fun ~key:loc ~data:root a ->
      Node.fold root ~backtrace_rev:[ loc ] ~init:a ~f)
  ;;

  let find t backtrace =
    match backtrace with
    | [] -> None
    | loc :: locs ->
      let%bind.Option root = Location.Map.find t.roots loc in
      Node.find root locs
  ;;
end

module Info = struct
  type t =
    { sample_rate : float
    ; word_size : int
    ; executable_name : string
    ; host_name : string
    ; ocaml_runtime_params : string
    ; pid : Int64.t
    ; start_time : Time_ns.Stable.Alternate_sexp.V1.t
    ; context : string option
    }
  [@@deriving sexp, bin_io]
end

type t =
  { graph : Graph.t
  ; filtered_graph : Graph.t option
  ; trie : Trie.t
  ; direction : Filter.direction
  ; total_allocations_unfiltered : Byte_units.Stable.V2.t
  ; info : Info.t option
  }
[@@deriving sexp, bin_io]

let empty =
  { graph = Graph.create []
  ; filtered_graph = None
  ; trie = Trie.empty
  ; direction = Explore_downwards_from_allocations
  ; total_allocations_unfiltered = Byte_units.zero
  ; info = None
  }
;;
