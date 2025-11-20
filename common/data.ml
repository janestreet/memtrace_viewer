open! Core
include Data_intf

module Call_site = struct
  module T = struct
    type t =
      { defname : string
      ; filename : string
      ; position : (string[@compare.ignore] [@hash.ignore])
      ; line : int
      ; start_char : int
      ; end_char : int
      }
    [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let create ~filename ~line ~start_char ~end_char ~defname =
    let position = sprintf "%d:%d-%d" line start_char end_char in
    { defname; filename; position; line; start_char; end_char }
  ;;

  let defname t = t.defname
  let filename t = t.filename
  let position t = t.position

  let to_string { defname; filename; position; _ } =
    sprintf "%s (%s:%s)" defname filename position
  ;;

  module Debug = struct
    type nonrec t = t

    let sexp_of_t t = Sexp.Atom (t |> to_string)
  end
end

module Allocation_site = struct
  module T = struct
    type t = Call_site.t [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_call_site t = t
  let defname = Call_site.defname
  let filename = Call_site.filename
  let position = Call_site.position
  let short_name t = sprintf "%s:%s" (filename t) (position t)

  let full_name t =
    sprintf "Allocation in %s (%s:%s)" (defname t) (filename t) (position t)
  ;;

  module Debug = struct
    type nonrec t = t

    let sexp_of_t t = Sexp.Atom (full_name t)
  end
end

module Function = struct
  module T = struct
    type t = { defname : string } [@@unboxed] [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make_binable (T)
  include Hashable.Make (T)

  let create ~defname = { defname }

  let full_name { defname } ~call_sites =
    match call_sites with
    | None -> defname
    | Some call_sites ->
      let locs_str =
        match call_sites with
        | [] ->
          (* possible if all call sites are actually allocation sites *)
          "no outgoing calls"
        | first_call_site :: remaining_call_sites ->
          (* Since the filenames are all the same, include the filename for the first call
             site only. *)
          let first_loc_str =
            sprintf
              "%s:%s"
              (Call_site.filename first_call_site)
              (Call_site.position first_call_site)
          in
          let remaining_loc_strs = List.map ~f:Call_site.position remaining_call_sites in
          String.concat ~sep:", " (first_loc_str :: remaining_loc_strs)
      in
      sprintf "%s (%s)" defname locs_str
  ;;

  let defname t = t.defname

  module Debug = struct
    type nonrec t = t

    let sexp_of_t t = Sexp.Atom (defname t)
  end
end

module Location = struct
  module T = struct
    type t =
      | Function of Function.t
      | Allocation_site of Allocation_site.t
      | Allocator
      | Toplevel
      | Dummy
    [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let create_function func = Function func
  let create_allocation_site alloc_site = Allocation_site alloc_site
  let allocator = Allocator
  let toplevel = Toplevel
  let dummy = Dummy

  let is_allocator = function
    | Allocator -> true
    | _ -> false
  ;;

  let is_toplevel = function
    | Toplevel -> true
    | _ -> false
  ;;

  let is_dummy = function
    | Dummy -> true
    | _ -> false
  ;;

  let is_special = function
    | Allocator | Toplevel | Dummy -> true
    | Function _ | Allocation_site _ -> false
  ;;

  let allocator_string = "(allocator)"
  let toplevel_string = "(toplevel)"
  let dummy_string = "(no location)"

  let defname = function
    | Function { defname; _ } -> defname
    | Allocation_site { defname; _ } -> defname
    | Allocator -> allocator_string
    | Toplevel -> toplevel_string
    | Dummy -> dummy_string
  ;;

  let full_name t ~call_sites =
    match t with
    | Function func -> Function.full_name func ~call_sites
    | Allocation_site alloc_site -> Allocation_site.full_name alloc_site
    | Allocator -> allocator_string
    | Toplevel -> toplevel_string
    | Dummy -> dummy_string
  ;;

  module Debug = struct
    type nonrec t = t =
      | Function of Function.Debug.t
      | Allocation_site of Call_site.Debug.t
      | Allocator
      | Toplevel
      | Dummy
    [@@deriving sexp_of]
  end
end

module Call_sites = struct
  type t = Call_site.t list Function.Table.t [@@deriving sexp]

  let create list =
    list
    |> List.filter_map ~f:(fun (func, call_sites) ->
      if List.is_empty call_sites
      then None
      else Some (func, call_sites |> List.sort ~compare:Call_site.compare))
    |> Function.Table.of_alist_exn
  ;;

  let for_function t func = Hashtbl.find t func |> Option.value ~default:[]

  let for_location t (loc : Location.t) =
    match loc with
    | Function func -> for_function t func
    | Allocation_site _ | Allocator | Toplevel | Dummy -> []
  ;;

  module Serialized = struct
    type t = (Function.t * Call_site.t list) list [@@deriving bin_io, sexp]

    let serialize t = t |> Hashtbl.to_alist
    let unserialize t = t |> Function.Table.of_alist_exn
  end
end

module Entry = struct
  type t =
    { allocations : Byte_units.Stable.V2.t
    ; direct_allocations : Byte_units.Stable.V2.t
    ; allocations_string : string
    ; percentage_string : string
    ; is_heavy : bool
    }
  [@@deriving sexp, bin_io, fields ~getters]

  let empty =
    let allocations = Byte_units.zero in
    let direct_allocations = Byte_units.zero in
    let is_heavy = false in
    let allocations_string = Byte_units.Short.to_string allocations in
    let percentage_string = "0%" in
    { allocations; direct_allocations; is_heavy; allocations_string; percentage_string }
  ;;

  let create ~total_allocations_in_trie ~allocations ~direct_allocations ~is_heavy =
    let allocations_string = Byte_units.Short.to_string allocations in
    let percentage = 100. *. Byte_units.(allocations // total_allocations_in_trie) in
    let percentage_string = Format.sprintf "%.1f%%" percentage in
    { allocations; direct_allocations; is_heavy; allocations_string; percentage_string }
  ;;

  module Debug = struct
    type nonrec t = t

    let sexp_of_t t = [%sexp_of: string] t.allocations_string
  end
end

module Metadata = struct
  type t = { total_allocations : Byte_units.Stable.V2.t } [@@deriving sexp, bin_io]
end

module Fragment_trie0 = Fragment_trie.Make (Location) (Entry) (Metadata)

module Backtrace = struct
  include Fragment_trie0.Backtrace

  let is_trivial = function
    | [] -> true
    | [ loc ] -> Location.is_special loc
    | _ -> false
  ;;
end

module Graph = struct
  type t =
    { points : (Time_ns.Span.t * Byte_units.Stable.V2.t) list
    ; max_x : Time_ns.Span.t
    ; max_y : Byte_units.Stable.V2.t
    }
  [@@deriving sexp, bin_io, fields ~getters]

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

module Fragment = struct
  include Fragment_trie0.Fragment

  let is_trivial t =
    is_empty t || (is_singleton t && Location.is_special (first ~orient:Callees t))
  ;;
end

module type Suffix_tree = Fragment_trie0.Suffix_tree

(* Since every fragment trie has the special singleton nodes representing the toplevel and
   the allocation site, we need to build a bit of structure even for an "empty" trie *)
module Empty_suffix_tree : Suffix_tree with type t = unit = struct
  module Location = Location
  module Entry = Entry

  module Node = struct
    module Id = struct
      module T = struct
        type t =
          | Empty
          | Allocator
          | Toplevel
        [@@deriving sexp, hash, compare]
      end

      include T
      include Hashable.Make (T)
    end

    type t = Id.t

    let id t = t
    let entry _t = Entry.empty

    let incoming_edge (t : t) : Location.t =
      match t with
      | Empty -> Dummy
      | Allocator -> Allocator
      | Toplevel -> Toplevel
    ;;

    let suffix (t : t) : t option =
      match t with
      | Empty -> None
      | Allocator | Toplevel -> Some Empty
    ;;

    let children (t : t) : (Location.t, t) List.Assoc.t =
      match t with
      | Empty -> [ Allocator, Allocator; Toplevel, Toplevel ]
      | Allocator | Toplevel -> []
    ;;

    let representative t = t

    module Debug = struct
      type nonrec t = Id.t [@@deriving sexp_of]
    end
  end

  type t = unit

  let root () : Node.t = Empty
end

module Fragment_trie = struct
  include Fragment_trie0.Trie

  let total_allocations t = (metadata t).total_allocations

  let empty =
    let suffix_tree : Empty_suffix_tree.t = () in
    let metadata : Metadata.t = { total_allocations = Byte_units.zero } in
    of_suffix_tree (module Empty_suffix_tree) suffix_tree ~metadata
  ;;

  let of_suffix_tree
    (type tree)
    (module Suffix_tree : Suffix_tree with type t = tree)
    (tree : tree)
    ~total_allocations
    =
    let metadata : Metadata.t = { total_allocations } in
    let is_empty_tree =
      let children_of_root = Suffix_tree.Node.children (Suffix_tree.root tree) in
      List.is_empty children_of_root
    in
    if is_empty_tree
    then (* Ensure that we still have the special singleton nodes *) empty
    else of_suffix_tree (module Suffix_tree) tree ~metadata
  ;;

  let toplevel_fragment t = find t [ Toplevel ] |> Option.value_exn
  let allocator_fragment t = find t [ Allocator ] |> Option.value_exn
end

module Info = struct
  type t =
    { sample_rate : float
    ; word_size : Byte_units.Stable.V2.t
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
  ; trie : Fragment_trie.t
  ; total_allocations_unfiltered : Byte_units.t
  ; peak_allocations : Byte_units.t
  ; peak_allocations_time : Time_ns.Span.t
  ; call_sites : Call_sites.t
  ; hot_paths : Fragment.t list
  ; hot_locations : Fragment.t list
  ; info : Info.t option
  }

module Serialized = struct
  type t =
    { graph : Graph.t
    ; filtered_graph : Graph.t option
    ; serialized_trie : Fragment_trie.Serialized.t
    ; total_allocations_unfiltered : Byte_units.Stable.V2.t
    ; peak_allocations : Byte_units.Stable.V2.t
    ; peak_allocations_time : Time_ns.Span.t
    ; serialized_call_sites : Call_sites.Serialized.t
    ; hot_path_backtraces : Backtrace.t list
    ; hot_locations : Location.t list
    ; info : Info.t option
    }
  [@@deriving sexp, bin_io]

  let serialize
    { hot_paths
    ; hot_locations
    ; graph
    ; filtered_graph
    ; trie
    ; total_allocations_unfiltered
    ; peak_allocations
    ; peak_allocations_time
    ; call_sites
    ; info
    }
    =
    let hot_path_backtraces = List.map ~f:Fragment.backtrace hot_paths in
    let hot_locations = List.map ~f:(Fragment.first ~orient:Callers) hot_locations in
    let serialized_trie = Fragment_trie.Serialized.serialize trie in
    let serialized_call_sites = Call_sites.Serialized.serialize call_sites in
    { hot_path_backtraces
    ; hot_locations
    ; graph
    ; filtered_graph
    ; serialized_trie
    ; total_allocations_unfiltered
    ; peak_allocations
    ; peak_allocations_time
    ; serialized_call_sites
    ; info
    }
  ;;

  let unserialize
    { hot_path_backtraces
    ; hot_locations
    ; graph
    ; filtered_graph
    ; serialized_trie
    ; total_allocations_unfiltered
    ; peak_allocations
    ; peak_allocations_time
    ; serialized_call_sites
    ; info
    }
    =
    let trie = Fragment_trie.Serialized.unserialize serialized_trie in
    let call_sites = Call_sites.Serialized.unserialize serialized_call_sites in
    let hot_paths =
      hot_path_backtraces
      |> List.map ~f:(fun backtrace ->
        Fragment_trie.find trie backtrace |> Option.value_exn)
    in
    let hot_locations =
      hot_locations
      |> List.map ~f:(fun location ->
        Fragment_trie.find_singleton trie location |> Option.value_exn)
    in
    { hot_paths
    ; hot_locations
    ; graph
    ; filtered_graph
    ; trie
    ; total_allocations_unfiltered
    ; peak_allocations
    ; peak_allocations_time
    ; call_sites
    ; info
    }
  ;;
end

include
  Sexpable.Of_sexpable
    (Serialized)
    (struct
      type nonrec t = t

      let to_sexpable = Serialized.serialize
      let of_sexpable = Serialized.unserialize
    end)

let empty =
  { graph = Graph.create []
  ; filtered_graph = None
  ; trie = Fragment_trie.empty
  ; total_allocations_unfiltered = Byte_units.zero
  ; peak_allocations = Byte_units.zero
  ; peak_allocations_time = Time_ns.Span.zero
  ; call_sites = Call_sites.create []
  ; hot_paths = []
  ; hot_locations = []
  ; info = None
  }
;;
