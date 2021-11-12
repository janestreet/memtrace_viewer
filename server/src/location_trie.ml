open! Core
open Memtrace_viewer_common
module Loc_hitters = Substring_heavy_hitters.Make (Location)

let find_heavy_hitters ~trace ~tolerance =
  let shh = Loc_hitters.create ~tolerance in
  Filtered_trace.iter ~mode:Preserve_backtraces trace (fun _time ev ->
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
        } ->
      (* Important: Memtrace order of stack frames is *toplevel first*, i.e. the opposite
         order to how stack traces are usually displayed. Reversing this here is not
         feasible as SHH's performance relies heavily on passing [common_prefix] through.

         XXX For now, I'm deciding to keep a consistent order for all in-memory
         representations of backtraces and flip it around only on display to the user.
         Another defensible choice would be to have the server side flip it around before
         sending to the client side (say, by having [Loc_hitters_as_suffix_trie] perform
         the reversal somehow), possibly relying on a wrapper type like
         [Data.Backtrace.Reversed.t] to keep them straight. (Could even have
         [Data.Backtrace.Toplevel_first.t] and [Data.Backtrace.Alloc_first.t].)
      *)
      let word =
        Array.sub
          backtrace_buffer
          ~pos:common_prefix
          ~len:(backtrace_length - common_prefix)
      in
      Loc_hitters.insert shh word ~count:nsamples ~common_prefix
    | Promote _ | Collect _ | End -> ());
  shh
;;

let bytes_of_samples ~rate ~word_size samples =
  let words = Float.of_int samples /. rate in
  Byte_units.scale word_size words
;;

module Loc_hitters_as_suffix_tree : sig
  include Data.Suffix_tree

  val of_loc_hitters
    :  loc_cache:Location.Cache.t
    -> significance_frequency:float
    -> sample_rate:float
    -> word_size:Byte_units.t
    -> Loc_hitters.t
    -> t
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
      ; significance_frequency : float
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

    let of_loc_hitters ~loc_cache ~significance_frequency ~sample_rate ~word_size shh =
      Loc_hitters.calculate_totals shh ~heaviness_frequency:significance_frequency;
      let suffix_cache = Hitter_subnode_id.Table.create () in
      { shh; significance_frequency; sample_rate; word_size; suffix_cache; loc_cache }
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

    let significance_threshold t =
      Byte_units.scale (total_allocations t) t.significance_frequency
    ;;

    let is_significant t node = Loc_hitters.contains_heavy t.shh node
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
      else
        Loc_hitters.Node.edge_char node edge_index
        |> Location.Cache.get_data trie.loc_cache
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
        let key_data = Location.Cache.get_data trie.loc_cache key in
        [ key_data, mk ~trie ~node ~edge_index:next_edge_index ]
      | None ->
        let root = Loc_hitters.root trie.shh in
        Loc_hitters.Node.fold_children node ~root ~init:[] ~f:(fun child children ->
          if Trie.is_significant trie child
          then (
            let key = Loc_hitters.Node.edge_char child 0 in
            let key_data = Location.Cache.get_data trie.loc_cache key in
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
           other words, [suffix] with its *entire* edge represents the suffix of [node] with
           its *entire* edge. In general, we start somewhere up [node]'s edge, so we begin
           by overapproximating: go to the end of [suffix] and then move up to compensate.
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

let trie_of_shh ~loc_cache ~rate ~word_size ~significance_frequency shh =
  let suffix_tree =
    Loc_hitters_as_suffix_tree.of_loc_hitters
      ~loc_cache
      ~sample_rate:rate
      ~word_size
      ~significance_frequency
      shh
  in
  Data.Fragment_trie.of_suffix_tree (module Loc_hitters_as_suffix_tree) suffix_tree
;;

let build ~trace ~loc_cache ~tolerance ~significance_frequency =
  let rate = Filtered_trace.sample_rate trace in
  let word_size = Filtered_trace.word_size trace in
  let shh = find_heavy_hitters ~trace ~tolerance in
  trie_of_shh shh ~loc_cache ~rate ~word_size ~significance_frequency
;;
