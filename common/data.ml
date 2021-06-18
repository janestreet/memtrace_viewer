open! Core
include Data_intf

module Location = struct
  module T = struct
    type t =
      | Allocation_site
      | Toplevel
      | Dummy
      | Function of
          { defname : string
          ; full_string : string
          ; loc_in_file : string
          }
    [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make_binable (T)
  include Hashable.Make (T)

  let create ~filename ~line ~start_char ~end_char ~defname =
    let loc_in_file = sprintf "(%s:%d:%d-%d)" filename line start_char end_char in
    let full_string = sprintf "%s %s" defname loc_in_file in
    Function { defname; loc_in_file; full_string }
  ;;

  let allocation_site = Allocation_site
  let toplevel = Toplevel
  let dummy = Dummy

  let is_allocation_site = function
    | Allocation_site -> true
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
    | Allocation_site | Toplevel | Dummy -> true
    | Function _ -> false
  ;;

  let allocation_site_string = "(allocation site)"
  let toplevel_string = "(toplevel)"
  let dummy_string = "(no location)"

  let defname = function
    | Function { defname; _ } -> defname
    | Allocation_site -> allocation_site_string
    | Toplevel -> toplevel_string
    | Dummy -> dummy_string
  ;;

  let loc_in_file = function
    | Function { loc_in_file; _ } -> loc_in_file
    | Allocation_site -> allocation_site_string
    | Toplevel -> toplevel_string
    | Dummy -> dummy_string
  ;;

  let full_name = function
    | Function { full_string; _ } -> full_string
    | Allocation_site -> allocation_site_string
    | Toplevel -> toplevel_string
    | Dummy -> dummy_string
  ;;

  module Debug = struct
    type nonrec t = t

    let sexp_of_t t = Sexp.Atom (full_name t)
  end
end

module Backtrace = struct
  module T = struct
    type t = Location.t list [@@deriving sexp, bin_io, compare, hash]
  end

  include T
  include Comparable.Make_binable (T)

  let is_trivial = function
    | [] -> true
    | [ loc ] -> Location.is_special loc
    | _ -> false
  ;;

  module Debug = struct
    type t = Location.Debug.t list [@@deriving sexp_of]
  end

  module Reversed : sig
    type t [@@deriving sexp, bin_io, compare, hash]

    include Comparable.S_binable with type t := t

    val nil : t
    val cons : Location.t -> t -> t
    val append : t -> t -> t
    val of_forward : Location.t list -> t
    val of_reversed_list : Location.t list -> t
    val elements : t -> Location.t list
    val head_and_tail : t -> (Location.t * t) option
    val hd : t -> Location.t option
    val tl : t -> t option

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end = struct
    module T = T
    include T
    include Comparable.Make_binable (T)

    let nil = []
    let cons loc t = loc :: t
    let append t1 t2 = t1 @ t2
    let of_forward t = List.rev t
    let of_reversed_list t = t
    let elements t = t

    let hd = function
      | [] -> None
      | loc :: _ -> Some loc
    ;;

    let tl = function
      | [] -> None
      | _ :: rest -> Some rest
    ;;

    let head_and_tail = function
      | [] -> None
      | loc :: t -> Some (loc, t)
    ;;

    module Debug = struct
      type t = Location.Debug.t list [@@deriving sexp_of]
    end
  end

  let of_reversed (t : Reversed.t) = List.rev (Reversed.elements t)
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

module Entry = struct
  type t =
    { allocations : Byte_units.Stable.V2.t
    ; direct_allocations : Byte_units.Stable.V2.t
    ; allocations_string : string
    ; percentage_string : string
    ; is_heavy : bool
    }
  [@@deriving sexp, bin_io, fields]

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
end

module Orientation = struct
  type t =
    | Callers
    | Callees
  [@@deriving sexp, equal]

  let flip = function
    | Callers -> Callees
    | Callees -> Callers
  ;;
end

module Fragment = struct
  module Id = Identifier.Make ()

  type t =
    { id : Id.t
    ; mutable entry : Entry.t
    ; mutable first_caller : Location.t
    ; last_callee : Location.t
    ; mutable retraction_by_caller : t
    ; mutable retraction_by_callee : t
    ; mutable extensions_by_caller : (Location.t, t) List.Assoc.t
    ; mutable extensions_by_callee : (Location.t, t) List.Assoc.t
    ; mutable representative : t
    ; mutable length : int
    }
  [@@deriving fields]

  let is_empty t = phys_equal t t.retraction_by_caller
  let same t1 t2 = phys_equal t1 t2

  let first t ~orient =
    match orient with
    | Orientation.Callers -> t.first_caller
    | Orientation.Callees -> t.last_callee
  ;;

  let retract t ~orient =
    if is_empty t
    then None
    else (
      let retraction =
        match orient with
        | Orientation.Callers -> t.retraction_by_caller
        | Orientation.Callees -> t.retraction_by_callee
      in
      Some retraction)
  ;;

  let rec retract_by t ~orient ~n =
    if n <= 0
    then Some t
    else (
      match retract t ~orient with
      | None -> None
      | Some t -> retract_by t ~orient ~n:(n - 1))
  ;;

  let backtrace t =
    let rec loop t acc =
      if is_empty t then acc else loop t.retraction_by_callee (t.last_callee :: acc)
    in
    loop t []
  ;;

  let is_trivial t =
    is_empty t || (is_empty t.retraction_by_callee && Location.is_special t.last_callee)
  ;;

  let backtrace_rev t =
    let rec loop t acc =
      if is_empty t
      then acc
      else loop t.retraction_by_caller (Backtrace.Reversed.cons t.first_caller acc)
    in
    loop t Backtrace.Reversed.nil
  ;;

  let rec deep_fold_callers t ~backtrace ~init ~f =
    let init = f ~backtrace ~fragment:t init in
    List.fold t.extensions_by_caller ~init ~f:(fun acc (loc, child) ->
      let backtrace = loc :: backtrace in
      deep_fold_callers child ~backtrace ~init:acc ~f)
  ;;

  let rec deep_fold_callees t ~backtrace_rev ~init ~f =
    let init = f ~backtrace_rev ~fragment:t init in
    List.fold t.extensions_by_callee ~init ~f:(fun acc (loc, child) ->
      let backtrace_rev = Backtrace.Reversed.cons loc backtrace_rev in
      deep_fold_callees child ~backtrace_rev ~init:acc ~f)
  ;;

  let one_frame_extensions t ~orient =
    match orient with
    | Orientation.Callers -> t.extensions_by_caller
    | Callees -> t.extensions_by_callee
  ;;

  let has_extensions t ~orient = not (List.is_empty (one_frame_extensions t ~orient))

  let extend t ~orient loc =
    List.Assoc.find ~equal:Location.equal (one_frame_extensions t ~orient) loc
  ;;

  let rec extend_by_callers t backtrace_rev =
    match Backtrace.Reversed.head_and_tail backtrace_rev with
    | None -> Some t
    | Some (loc, locs) ->
      let%bind.Option child = extend ~orient:Callers t loc in
      extend_by_callers child locs
  ;;

  let rec extend_by_callees t backtrace =
    match backtrace with
    | [] -> Some t
    | loc :: locs ->
      let%bind.Option child = extend ~orient:Callees t loc in
      extend_by_callees child locs
  ;;

  let is_extension t ~extension ~orient =
    let n = length extension - length t in
    if n < 0
    then false
    else (
      match retract_by ~orient ~n extension with
      | None -> assert false
      | Some extension -> same extension t)
  ;;

  module Debug = struct
    type nonrec t = t

    let sexp_of_t t =
      [%message
        ""
          ~id:(t.id : Id.t)
          ~allocations:(t.entry.allocations : Byte_units.t)
          ~backtrace:(backtrace t : Backtrace.Debug.t)]
    ;;
  end

  module Oriented = struct
    type nonrec t =
      { fragment : t
      ; orient : Orientation.t
      }

    let fragment { fragment; _ } = fragment
    let orient { orient; _ } = orient
    let first { fragment; orient } = first fragment ~orient

    let retract { fragment; orient } =
      let%map.Option fragment = retract fragment ~orient in
      { fragment; orient }
    ;;

    let retract_by { fragment; orient } ~n =
      let%map.Option fragment = retract_by fragment ~orient ~n in
      { fragment; orient }
    ;;

    let one_frame_extensions { fragment; orient } =
      one_frame_extensions fragment ~orient
      |> List.Assoc.map ~f:(fun fragment -> { fragment; orient })
    ;;

    let extend { fragment; orient } loc =
      let%map.Option fragment = extend fragment ~orient loc in
      { fragment; orient }
    ;;

    let has_extensions { fragment; orient } = has_extensions fragment ~orient

    module Debug = struct
      type nonrec t = t =
        { fragment : Debug.t
        ; orient : Orientation.t
        }
      [@@deriving sexp_of]
    end
  end

  let oriented fragment ~orient = { Oriented.fragment; orient }

  module Iterator = struct
    (* We represent a position within a fragment the prefix ending at that
       position and the suffix ending at that position:

       {v
         |ABCDEFGHIJKLMNOPQRSTUVWXYZ| fragment
          ________I_________________  position
         |ABCDEFGHI|________________  prefix
         ________|IJKLMNOPQRSTUVWXYZ| suffix
       v} *)
    type nonrec t =
      { prefix : t
      ; suffix : t
      }

    let prefix { prefix; _ } = prefix
    let suffix { suffix; _ } = suffix
    let location { suffix; _ } = first ~orient:Callers suffix

    let next { prefix; suffix } =
      match retract ~orient:Callers suffix with
      | None -> assert false
      | Some suffix ->
        if is_empty suffix
        then None
        else (
          let next_loc = first ~orient:Callers suffix in
          let prefix =
            match extend ~orient:Callees prefix next_loc with
            | Some fragment -> fragment
            | None -> assert false
          in
          Some { prefix; suffix })
    ;;

    let prev { prefix; suffix } =
      match retract ~orient:Callees prefix with
      | None -> assert false
      | Some prefix ->
        let next_loc = first ~orient:Callees prefix in
        if is_empty prefix
        then None
        else (
          let suffix =
            match extend ~orient:Callers suffix next_loc with
            | Some fragment -> fragment
            | None -> assert false
          in
          Some { prefix; suffix })
    ;;

    module Trace = struct
      module T = struct
        type t =
          { prefix_trace : Backtrace.Reversed.t
          ; suffix_trace : Backtrace.t
          }
        [@@deriving sexp, bin_io, compare, hash]
      end

      include T
      include Comparable.Make_binable (T)
    end

    let trace { prefix; suffix } =
      let prefix_trace = backtrace_rev prefix in
      let suffix_trace = backtrace suffix in
      { Trace.prefix_trace; suffix_trace }
    ;;
  end

  let iterator_start t =
    if is_empty t
    then None
    else (
      let rec loop prefix =
        match retract ~orient:Callees prefix with
        | None -> assert false
        | Some prev -> if is_empty prev then prefix else loop prev
      in
      let prefix = loop t in
      let suffix = t in
      Some { Iterator.prefix; suffix })
  ;;

  let iterator_end t =
    if is_empty t
    then None
    else (
      let rec loop suffix =
        match retract ~orient:Callers suffix with
        | None -> assert false
        | Some next -> if is_empty next then suffix else loop next
      in
      let suffix = loop t in
      let prefix = t in
      Some { Iterator.prefix; suffix })
  ;;
end

module Fragment_trie = struct
  type t =
    { root : Fragment.t
    ; children_of_root : Fragment.t Location.Table.t
    ; total_allocations : Byte_units.t
    }

  module type Suffix_tree =
    Data_intf.Suffix_tree with type entry := Entry.t and type location := Location.t

  let create ~(root : Fragment.t) ~total_allocations =
    assert (
      List.equal
        (Tuple2.equal ~eq1:Location.equal ~eq2:Fragment.same)
        root.extensions_by_caller
        root.extensions_by_callee);
    let children_of_root = Location.Table.of_alist_exn root.extensions_by_callee in
    let t = { root; children_of_root; total_allocations } in
    t
  ;;

  let empty_fragment t = t.root

  let allocation_site_fragment t =
    Location.Table.find_exn t.children_of_root Location.allocation_site
  ;;

  let toplevel_fragment t = Location.Table.find_exn t.children_of_root Location.toplevel

  let empty =
    let id_gen = Fragment.Id.Generator.create () in
    let rec allocation_site : Fragment.t =
      { id = Fragment.Id.Generator.generate id_gen
      ; entry = Entry.empty
      ; first_caller = Allocation_site
      ; last_callee = Allocation_site
      ; retraction_by_caller = root
      ; retraction_by_callee = root
      ; extensions_by_caller = []
      ; extensions_by_callee = []
      ; representative = allocation_site
      ; length = 1
      }
    and toplevel : Fragment.t =
      { id = Fragment.Id.Generator.generate id_gen
      ; entry = Entry.empty
      ; first_caller = Toplevel
      ; last_callee = Toplevel
      ; retraction_by_caller = root
      ; retraction_by_callee = root
      ; extensions_by_caller = []
      ; extensions_by_callee = []
      ; representative = toplevel
      ; length = 1
      }
    and children_of_root =
      [ Location.allocation_site, allocation_site; Location.toplevel, toplevel ]
    and root : Fragment.t =
      { id = Fragment.Id.Generator.generate id_gen
      ; entry = Entry.empty
      ; first_caller = Dummy
      ; last_callee = Dummy
      ; retraction_by_caller = root
      ; retraction_by_callee = root
      ; extensions_by_caller = children_of_root
      ; extensions_by_callee = children_of_root
      ; representative = root
      ; length = 0
      }
    in
    let total_allocations = Byte_units.zero in
    create ~root ~total_allocations
  ;;

  let of_suffix_tree
        (type tree)
        (module Tree : Suffix_tree with type t = tree)
        (tree : tree)
    : t
    =
    let id_gen = Fragment.Id.Generator.create () in
    let old_root_node = Tree.root tree in
    let old_root_children = Tree.Node.children old_root_node in
    if List.is_empty old_root_children
    then empty
    else (
      let cache : Fragment.t Tree.Node.Id.Table.t = Tree.Node.Id.Table.create () in
      let rec new_root_node =
        { Fragment.id = Fragment.Id.Generator.generate id_gen
        ; entry = Tree.Node.entry old_root_node
        ; first_caller = Dummy
        ; last_callee = Dummy
        ; retraction_by_caller = new_root_node
        ; retraction_by_callee = new_root_node
        ; extensions_by_caller = []
        ; extensions_by_callee = []
        ; representative = new_root_node
        ; length = 0
        }
      in
      Tree.Node.Id.Table.add_exn
        cache
        ~key:(Tree.Node.id old_root_node)
        ~data:new_root_node;
      let node_of old_node =
        Tree.Node.Id.Table.find_or_add cache (Tree.Node.id old_node) ~default:(fun () ->
          let id = Fragment.Id.Generator.generate id_gen in
          let entry = Tree.Node.entry old_node in
          let first_caller = (* to be corrected *) Location.Dummy in
          let last_callee = Tree.Node.incoming_edge old_node in
          let retraction_by_caller = (* to be corrected *) new_root_node in
          let retraction_by_callee = (* to be corrected *) new_root_node in
          let extensions_by_caller = (* to be corrected *) [] in
          let extensions_by_callee = (* to be corrected *) [] in
          let representative = (* to be corrected *) new_root_node in
          let length = 0 in
          { Fragment.id
          ; entry
          ; first_caller
          ; last_callee
          ; retraction_by_caller
          ; retraction_by_callee
          ; extensions_by_caller
          ; extensions_by_callee
          ; representative
          ; length
          })
      in
      let rec translate ~length ~first_edge ~new_parent (last_edge, old_node) =
        let new_node = node_of old_node in
        new_node.first_caller <- first_edge;
        new_node.length <- length;
        new_node.extensions_by_callee
        <- List.map
             ~f:(translate ~length:(length + 1) ~first_edge ~new_parent:new_node)
             (Tree.Node.children old_node);
        new_node.retraction_by_callee <- new_parent;
        (* This is the node in which this node appears among the [extensions_by_caller]. *)
        let parent_by_caller =
          (* Since the suffix trie represents stacks in caller-first order, the parent by
             caller is the suffix. *)
          match Tree.Node.suffix old_node with
          | Some old_suffix -> node_of old_suffix
          | None ->
            raise_s
              [%message
                "non-root node has no suffix"
                  ~id:(Tree.Node.id old_node : Tree.Node.Id.t)
                  ~debug:(old_node : Tree.Node.Debug.t)]
        in
        new_node.retraction_by_caller <- parent_by_caller;
        parent_by_caller.extensions_by_caller
        <- (first_edge, new_node) :: parent_by_caller.extensions_by_caller;
        new_node.representative <- node_of (Tree.Node.representative old_node);
        last_edge, new_node
      in
      let children_of_root =
        List.map old_root_children ~f:(fun ((first_edge, _) as child) ->
          translate ~length:1 ~first_edge ~new_parent:new_root_node child)
      in
      new_root_node.extensions_by_caller <- children_of_root;
      new_root_node.extensions_by_callee <- children_of_root;
      let total_allocations = Tree.total_allocations tree in
      create ~root:new_root_node ~total_allocations)
  ;;

  let deep_fold_callers t ~init ~f =
    Fragment.deep_fold_callers t.root ~backtrace:[] ~init ~f
  ;;

  let deep_fold_callees t ~init ~f =
    Fragment.deep_fold_callees t.root ~backtrace_rev:Backtrace.Reversed.nil ~init ~f
  ;;

  let fold_singletons t ~init ~f =
    Location.Table.fold t.children_of_root ~init ~f:(fun ~key ~data ->
      f ~location:key ~fragment:data)
  ;;

  let total_allocations t = t.total_allocations

  let find t backtrace =
    match backtrace with
    | [] -> Some t.root
    | first :: backtrace ->
      let%bind.Option child = Location.Table.find t.children_of_root first in
      Fragment.extend_by_callees child backtrace
  ;;

  let find_rev t backtrace_rev =
    match Backtrace.Reversed.head_and_tail backtrace_rev with
    | None -> Some t.root
    | Some (first, backtrace_rev) ->
      let%bind.Option child = Location.Table.find t.children_of_root first in
      Fragment.extend_by_callers child backtrace_rev
  ;;

  let find_singleton t location = Location.Table.find t.children_of_root location

  let find_iterator t { Fragment.Iterator.Trace.prefix_trace; suffix_trace } =
    let%bind.Option prefix = find_rev t prefix_trace in
    let%bind.Option suffix = find t suffix_trace in
    (* Check that the iterator is still valid *)
    let%map.Option (_ : Fragment.t) =
      let prefix_tail = Option.value_exn (Backtrace.Reversed.tl prefix_trace) in
      Fragment.extend_by_callers suffix prefix_tail
    in
    { Fragment.Iterator.prefix; suffix }
  ;;

  module Serialized_form = struct
    module Unserialized_fragment = Fragment

    module Fragment = struct
      type t =
        { id : Fragment.Id.t
        ; entry : Entry.t
        ; first_caller : Location.t
        ; last_callee : Location.t
        ; retraction_id_by_caller : Fragment.Id.t
        ; extension_ids_by_caller : (Location.t, Fragment.Id.t) List.Assoc.t
        ; extensions_by_callee : (Location.t, t) List.Assoc.t
        ; representative_id : Fragment.Id.t
        ; length : int
        }
      [@@deriving bin_io, sexp]

      let rec of_trie_node
                ({ id
                 ; entry
                 ; first_caller
                 ; last_callee
                 ; retraction_by_caller
                 ; retraction_by_callee = _
                 ; extensions_by_caller
                 ; extensions_by_callee
                 ; representative
                 ; length
                 } :
                   Fragment.t)
        =
        let retraction_id_by_caller = retraction_by_caller.id in
        let extension_ids_by_caller =
          List.Assoc.map extensions_by_caller ~f:(fun (child : Fragment.t) -> child.id)
        in
        let extensions_by_callee = List.Assoc.map ~f:of_trie_node extensions_by_callee in
        let representative_id = representative.id in
        { id
        ; entry
        ; first_caller
        ; last_callee
        ; retraction_id_by_caller
        ; extension_ids_by_caller
        ; extensions_by_callee
        ; representative_id
        ; length
        }
      ;;
    end

    type trie = t

    type t =
      { root : Fragment.t
      ; total_allocations : Byte_units.Stable.V2.t
      }
    [@@deriving sexp, bin_io]

    let of_trie (trie : trie) =
      let root = Fragment.of_trie_node trie.root in
      let total_allocations = trie.total_allocations in
      { root; total_allocations }
    ;;

    let to_trie t : trie =
      (* Two passes:

         1. Create the trie by a simple traversal, leaving the caller children and
         back pointers empty.
         2. Fill in the caller children, now that we have a node for each id. *)

      (* Each unserialized node, along with the edges and ids of its prefix children;
         we use this to perform pass 2 *)
      let fragment_cache : Unserialized_fragment.t Unserialized_fragment.Id.Table.t =
        Unserialized_fragment.Id.Table.create ()
      in
      let find_in_cache desc id =
        match Unserialized_fragment.Id.Table.find fragment_cache id with
        | Some fragment -> fragment
        | None -> raise_s [%message desc (id : Unserialized_fragment.Id.t)]
      in
      let rec unserialize_without_callers
                ~retraction_by_callee
                ({ id
                 ; entry
                 ; first_caller
                 ; last_callee
                 ; retraction_id_by_caller = _
                 ; extension_ids_by_caller = _
                 ; extensions_by_callee
                 ; representative_id = _
                 ; length
                 } :
                   Fragment.t)
        =
        let rec fragment : Unserialized_fragment.t =
          { id
          ; entry
          ; first_caller
          ; last_callee
          ; retraction_by_caller = (* to be corrected *) fragment
          ; retraction_by_callee
          ; extensions_by_caller = (* to be corrected *) []
          ; extensions_by_callee = (* corrected immediately below *) []
          ; representative = (* to be corrected *) fragment
          ; length
          }
        in
        fragment.extensions_by_callee
        <- List.Assoc.map
             ~f:(unserialize_without_callers ~retraction_by_callee:fragment)
             extensions_by_callee;
        Unserialized_fragment.Id.Table.add_exn fragment_cache ~key:id ~data:fragment;
        fragment
      in
      let rec fill_in_callers
                (new_fragment : Unserialized_fragment.t)
                (old_fragment : Fragment.t)
        =
        new_fragment.retraction_by_caller
        <- find_in_cache "retraction_by_caller" old_fragment.retraction_id_by_caller;
        let extensions_by_caller =
          List.Assoc.map
            ~f:(find_in_cache "extensions_by_caller")
            old_fragment.extension_ids_by_caller
        in
        new_fragment.extensions_by_caller <- extensions_by_caller;
        new_fragment.representative
        <- find_in_cache "representative" old_fragment.representative_id;
        List.iter2_exn
          new_fragment.extensions_by_callee
          old_fragment.extensions_by_callee
          ~f:(fun (_, new_child) (_, old_child) -> fill_in_callers new_child old_child)
      in
      let rec root =
        { Unserialized_fragment.id = t.root.id
        ; entry = t.root.entry
        ; first_caller = t.root.first_caller
        ; last_callee = t.root.last_callee
        ; retraction_by_caller = root
        ; retraction_by_callee = root
        ; extensions_by_caller = []
        ; extensions_by_callee = []
        ; representative = root
        ; length = 0
        }
      in
      Unserialized_fragment.Id.Table.add_exn fragment_cache ~key:root.id ~data:root;
      root.extensions_by_callee
      <- List.Assoc.map
           ~f:(unserialize_without_callers ~retraction_by_callee:root)
           t.root.extensions_by_callee;
      fill_in_callers root t.root;
      let total_allocations = t.total_allocations in
      create ~root ~total_allocations
    ;;
  end

  include
    Sexpable.Of_sexpable
      (Serialized_form)
      (struct
        type nonrec t = t

        let to_sexpable = Serialized_form.of_trie
        let of_sexpable = Serialized_form.to_trie
      end)

  include
    Binable.Of_binable_with_uuid
      (Serialized_form)
      (struct
        type nonrec t = t

        let to_binable = Serialized_form.of_trie
        let of_binable = Serialized_form.to_trie

        let caller_identity =
          "8a41d09e-dd79-41c7-99ea-c03ff2ee8cdf" |> Bin_prot.Shape.Uuid.of_string
        ;;
      end)
end

module type Suffix_tree = Fragment_trie.Suffix_tree

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
  ; trie : Fragment_trie.t
  ; total_allocations_unfiltered : Byte_units.Stable.V2.t
  ; hot_paths : Fragment.t list
  ; hot_call_sites : Fragment.t list
  ; info : Info.t option
  }

module Serialized_form = struct
  type t =
    { graph : Graph.t
    ; filtered_graph : Graph.t option
    ; trie : Fragment_trie.t
    ; total_allocations_unfiltered : Byte_units.Stable.V2.t
    ; hot_path_backtraces : Backtrace.t list
    ; hot_call_site_locations : Location.t list
    ; info : Info.t option
    }
  [@@deriving sexp, bin_io]

  let serialize
        { hot_paths
        ; hot_call_sites
        ; graph
        ; filtered_graph
        ; trie
        ; total_allocations_unfiltered
        ; info
        }
    =
    let hot_path_backtraces = List.map ~f:Fragment.backtrace hot_paths in
    let hot_call_site_locations =
      List.map ~f:(Fragment.first ~orient:Callers) hot_call_sites
    in
    { hot_path_backtraces
    ; hot_call_site_locations
    ; graph
    ; filtered_graph
    ; trie
    ; total_allocations_unfiltered
    ; info
    }
  ;;

  let unserialize
        { hot_path_backtraces
        ; hot_call_site_locations
        ; graph
        ; filtered_graph
        ; trie
        ; total_allocations_unfiltered
        ; info
        }
    =
    let hot_paths =
      hot_path_backtraces
      |> List.map ~f:(fun backtrace ->
        Fragment_trie.find trie backtrace |> Option.value_exn)
    in
    let hot_call_sites =
      hot_call_site_locations
      |> List.map ~f:(fun location ->
        Fragment_trie.find_singleton trie location |> Option.value_exn)
    in
    { hot_paths
    ; hot_call_sites
    ; graph
    ; filtered_graph
    ; trie
    ; total_allocations_unfiltered
    ; info
    }
  ;;
end

include
  Sexpable.Of_sexpable
    (Serialized_form)
    (struct
      type nonrec t = t

      let to_sexpable = Serialized_form.serialize
      let of_sexpable = Serialized_form.unserialize
    end)

include
  Binable.Of_binable_with_uuid
    (Serialized_form)
    (struct
      type nonrec t = t

      let to_binable = Serialized_form.serialize
      let of_binable = Serialized_form.unserialize

      let caller_identity =
        "9232d992-2e98-457d-849d-0e4f1ec460a8" |> Bin_prot.Shape.Uuid.of_string
      ;;
    end)

let empty =
  { graph = Graph.create []
  ; filtered_graph = None
  ; trie = Fragment_trie.empty
  ; total_allocations_unfiltered = Byte_units.zero
  ; hot_paths = []
  ; hot_call_sites = []
  ; info = None
  }
;;
