open! Core
open Memtrace_viewer_common
module Loc_hitters = Substring_heavy_hitters.Make (Location)

let find_heavy_hitters ~trace ~tolerance ~significance_frequency
  : Loc_hitters.t * Filtered_trace.Call_sites.t
  =
  let shh = Loc_hitters.create ~tolerance in
  let first = ref true in
  let call_sites =
    Filtered_trace.iter_and_gather_call_sites
      ~mode:Preserve_backtraces
      trace
      (fun _time ev ->
         match ev with
         | Alloc
             { obj_id = _
             ; nsamples
             ; source = _
             ; single_allocation_size = _
             ; size = _
             ; backtrace_buffer
             ; backtrace_length
             ; common_prefix
             ; domain = _
             } ->
           (* Important: Memtrace order of stack frames is *toplevel first*, i.e. the
              opposite order to how stack traces are usually displayed. Reversing this
              here is not feasible as SHH's performance relies heavily on passing
              [common_prefix] through.

              XXX For now, I'm deciding to keep a consistent order for all in-memory
              representations of backtraces and flip it around only on display to the
              user. Another defensible choice would be to have the server side flip it
              around before sending to the client side (say, by having
              [Loc_hitters_as_suffix_trie] perform the reversal somehow), possibly relying
              on a wrapper type like [Data.Backtrace.Reversed.t] to keep them straight.
              (Could even have [Data.Backtrace.Toplevel_first.t] and
              [Data.Backtrace.Alloc_first.t].)

              Make enough space for the toplevel location, the backtrace minus the common
              prefix, and the allocator location.
           *)
           let backtrace_length_after_common_prefix = backtrace_length - common_prefix in
           let space_for_added_toplevel =
             (* Make room for the [Location.toplevel] in the very first backtrace.
                Subsequent backtraces will include it in the common prefix. *)
             if !first then 1 else 0
           in
           let space_for_added_allocator =
             (* Always there at the end *)
             1
           in
           let word_len =
             space_for_added_toplevel
             + backtrace_length_after_common_prefix
             + space_for_added_allocator
           in
           let word = Array.create ~len:word_len Location.dummy in
           if !first then word.(0) <- Location.toplevel;
           word.(word_len - 1) <- Location.allocator;
           Array.blit
             ~src:backtrace_buffer
             ~src_pos:common_prefix
             ~dst:word
             ~dst_pos:space_for_added_toplevel
             ~len:backtrace_length_after_common_prefix;
           let common_prefix =
             if !first
             then 0
             else
               (* Account for the [Location.toplevel] we added the very first time *)
               common_prefix + 1
           in
           first := false;
           Loc_hitters.insert shh word ~count:nsamples ~common_prefix
         | Promote _ | Collect _ | End -> ())
  in
  Loc_hitters.calculate_totals shh ~heaviness_frequency:significance_frequency;
  shh, call_sites
;;

let bytes_of_samples ~rate ~word_size samples =
  let words = Float.of_int samples /. rate in
  Byte_units.scale word_size words
;;

(* Make sure we're using the same criteria when pruning the trie or dropping call sites *)
let is_significant ~shh node = Loc_hitters.contains_heavy shh node

module Loc_hitters_as_suffix_tree : sig
  include Data.Suffix_tree

  val of_loc_hitters
    :  loc_cache:Location.Cache.t
    -> sample_rate:float
    -> word_size:Byte_units.t
    -> Loc_hitters.t
    -> t

  val total_allocations : t -> Byte_units.t
end = struct
  module Hitter_subnode_id : sig
    type t

    include Hashable.S with type t := t
    include Sexpable.S with type t := t

    val of_ : node:Loc_hitters.Node.t -> edge_index:int -> t
  end = struct
    module T = struct
      type t =
        { node_id : int
        ; edge_index : int
        }
      [@@deriving hash, compare, sexp]
    end

    include T
    include Hashable.Make (T)

    let of_ ~node ~edge_index =
      { node_id = (Loc_hitters.Node.id node :> int); edge_index }
    ;;
  end

  module Trie = struct
    type t =
      { shh : Loc_hitters.t
      ; sample_rate : float
      ; word_size : Byte_units.t
      ; suffix_cache : node Hitter_subnode_id.Table.t
      ; loc_cache : Location.Cache.t
      }

    and node =
      { trie : t
      ; node : Loc_hitters.Node.t
      ; edge_index : int
      }

    let of_loc_hitters ~loc_cache ~sample_rate ~word_size shh =
      let suffix_cache = Hitter_subnode_id.Table.create () in
      { shh; sample_rate; word_size; suffix_cache; loc_cache }
    ;;

    let real_root t = Loc_hitters.root t.shh

    let root t =
      let node = real_root t |> Loc_hitters.Node.Root.node in
      { trie = t; node; edge_index = -1 }
    ;;

    let bytes_of_samples t samples =
      bytes_of_samples samples ~rate:t.sample_rate ~word_size:t.word_size
    ;;

    let total_allocations t = Loc_hitters.total_count t.shh |> bytes_of_samples t
    let loc_data t loc = Location.Cache.get_loc_data t.loc_cache loc
  end

  module Node = struct
    module Id = Hitter_subnode_id

    module T : sig
      type t = Trie.node = private
        { trie : Trie.t
        ; node : Loc_hitters.Node.t
        ; edge_index : int
        }

      val mk : trie:Trie.t -> node:Loc_hitters.Node.t -> edge_index:int -> t
    end = struct
      type t = Trie.node =
        { trie : Trie.t
        ; node : Loc_hitters.Node.t
        ; edge_index : int
        }

      let mk ~trie ~node ~edge_index =
        let () = assert (edge_index < Loc_hitters.Node.edge_length node) in
        { trie; node; edge_index }
      ;;
    end

    include T

    let incoming_edge { trie; node; edge_index } =
      if edge_index < 0
      then (* Root node *)
        Data.Location.dummy
      else Loc_hitters.Node.edge_char node edge_index |> Trie.loc_data trie
    ;;

    let next_pos_along_edge node edge_index =
      let next_edge_index = edge_index + 1 in
      if next_edge_index < Loc_hitters.Node.edge_length node
      then Some next_edge_index
      else None
    ;;

    let children { trie; node; edge_index } =
      match next_pos_along_edge node edge_index with
      | Some next_edge_index ->
        let key = Loc_hitters.Node.edge_char node next_edge_index in
        let key_data = Trie.loc_data trie key in
        [ key_data, mk ~trie ~node ~edge_index:next_edge_index ]
      | None ->
        let root = Loc_hitters.root trie.shh in
        Loc_hitters.Node.fold_children node ~root ~init:[] ~f:(fun child children ->
          if is_significant ~shh:trie.shh child
          then (
            let key = Loc_hitters.Node.edge_char child 0 in
            let key_data = Trie.loc_data trie key in
            (key_data, mk ~trie ~node:child ~edge_index:0) :: children)
          else children)
    ;;

    module Debug = struct
      type nonrec t = t

      let sexp_of_t { trie = _; node; edge_index } =
        [%message (edge_index : int) (node : Loc_hitters.Node.Debug_full.t)]
      ;;
    end

    let find_suffix ~trie node edge_index =
      let is_root = phys_equal (Loc_hitters.Node.parent node) node in
      if is_root
      then None
      else (
        (* The semantics of the suffix link is defined according to the entire edge. In
           other words, [suffix] with its *entire* edge represents the suffix of [node]
           with its *entire* edge. In general, we start somewhere up [node]'s edge, so we
           begin by overapproximating: go to the end of [suffix] and then move up to
           compensate.
        *)
        assert (Loc_hitters.Node.has_suffix node);
        let suffix = Loc_hitters.Node.suffix node in
        let distance_to_move_up = Loc_hitters.Node.edge_length node - 1 - edge_index in
        let rec loop ~suffix ~distance_to_move_up =
          let is_root = phys_equal suffix (Loc_hitters.Node.parent suffix) in
          let edge_length = Loc_hitters.Node.edge_length suffix in
          let bottom_edge_index = edge_length - 1 in
          if is_root
          then (
            assert (distance_to_move_up = 0);
            Some (Trie.root trie))
          else if distance_to_move_up <= bottom_edge_index
          then (
            let edge_index = bottom_edge_index - distance_to_move_up in
            Some (mk ~trie ~node:suffix ~edge_index))
          else (
            let suffix = Loc_hitters.Node.parent suffix in
            let distance_to_move_up = distance_to_move_up - edge_length in
            loop ~suffix ~distance_to_move_up)
        in
        loop ~suffix ~distance_to_move_up)
    ;;

    let suffix { trie; node; edge_index } = find_suffix ~trie node edge_index

    let entry { trie; node; edge_index = _ } =
      let total_allocations_in_trie = Trie.total_allocations trie in
      let allocations = Loc_hitters.Node.total_count node |> Trie.bytes_of_samples trie in
      let direct_allocations =
        Loc_hitters.Node.light_count node |> Trie.bytes_of_samples trie
      in
      let is_heavy = Loc_hitters.is_heavy trie.shh node in
      Data.Entry.create
        ~total_allocations_in_trie
        ~allocations
        ~direct_allocations
        ~is_heavy
    ;;

    let id { trie = _; node; edge_index } = Hitter_subnode_id.of_ ~node ~edge_index

    let representative { trie; node; edge_index = _ } =
      let repr = Loc_hitters.Node.representative node in
      (* The representative is always the longest fragment in its class, so pick the
         bottom subnode *)
      let edge_index = Loc_hitters.Node.edge_length repr - 1 in
      mk ~trie ~node:repr ~edge_index
    ;;
  end

  include Trie
end

let find_call_node ~caller ~callee ~shh =
  let root = Loc_hitters.root shh in
  let%bind.Option caller_node =
    Loc_hitters.Node.get_child_opt ~root (Loc_hitters.Node.Root.node root) caller
  in
  if Loc_hitters.Node.edge_length caller_node > 1
  then
    if Location.equal callee (Loc_hitters.Node.edge_char caller_node 1)
    then Some caller_node
    else None
  else Loc_hitters.Node.get_child_opt ~root caller_node callee
;;

let keep_call_site ~caller ~callee ~shh ~loc_cache =
  let root = Loc_hitters.root shh in
  (* This heuristic tends to drop call sites that only occur at recursion depth >1, since
     the true caller will have been filtered out of the backtrace. In the case of the
     actual allocation, though, we can compensate simply by taking the total over all
     callers, since there can only have been the one true caller. *)
  let call_node =
    match Location.Cache.get_loc_data loc_cache callee with
    | Allocation_site _ ->
      Loc_hitters.Node.get_child_opt ~root (Loc_hitters.Node.Root.node root) callee
    | Function _ | Allocator | Toplevel | Dummy -> find_call_node ~caller ~callee ~shh
  in
  match call_node with
  | None -> false
  | Some node -> is_significant ~shh node
;;

let trie_of_shh ~loc_cache ~rate ~word_size ~all_call_sites shh =
  let suffix_tree =
    Loc_hitters_as_suffix_tree.of_loc_hitters ~loc_cache ~sample_rate:rate ~word_size shh
  in
  let total_allocations = Loc_hitters_as_suffix_tree.total_allocations suffix_tree in
  let trie =
    Data.Fragment_trie.of_suffix_tree
      (module Loc_hitters_as_suffix_tree)
      suffix_tree
      ~total_allocations
  in
  let call_sites =
    all_call_sites
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (caller, all_call_sites_in_caller) ->
      let call_sites_and_callees = Hashtbl.to_alist all_call_sites_in_caller in
      let call_sites =
        List.filter_map call_sites_and_callees ~f:(fun (call_site, callees) ->
          if Hash_set.exists callees ~f:(fun callee ->
               keep_call_site ~caller ~callee ~shh ~loc_cache)
          then Some call_site
          else None)
      in
      let call_sites =
        match call_sites with
        | _ :: _ -> call_sites
        | [] ->
          (* Everything was pruned, but we'd like to list at least _one_ call site, so
             pick the first one *)
          let compare cs1 cs2 =
            let get_data = Location.Cache.get_call_site_data loc_cache in
            Data.Call_site.compare (get_data cs1) (get_data cs2)
          in
          (match List.sort call_sites ~compare with
           | call_site :: _ -> [ call_site ]
           | [] -> (* ??? *) [])
      in
      let call_sites =
        List.map ~f:(Location.Cache.get_call_site_data loc_cache) call_sites
      in
      let caller =
        match Location.Cache.get_loc_data loc_cache caller with
        | Function data -> data
        | (Allocation_site _ | Toplevel | Allocator | Dummy) as data ->
          raise_s
            [%message "Unexpected location data for caller" (data : Data.Location.t)]
      in
      caller, call_sites)
    |> Data.Call_sites.create
  in
  trie, call_sites
;;

let build ~trace ~loc_cache ~tolerance ~significance_frequency =
  let rate = Filtered_trace.sample_rate trace in
  let word_size = Filtered_trace.word_size trace in
  let shh, all_call_sites =
    find_heavy_hitters ~trace ~tolerance ~significance_frequency
  in
  trie_of_shh shh ~loc_cache ~rate ~word_size ~all_call_sites
;;
