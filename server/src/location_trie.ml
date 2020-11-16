open! Core_kernel
open Memtrace
open Memtrace_viewer_common
module Loc_hitters = Hierarchical_heavy_hitters.Make (Location)

let count ~trace ~error ~direction =
  let hhh = Loc_hitters.create error in
  Filtered_trace.iter ~parse_backtraces:true trace (fun _time ev ->
    match ev with
    | Alloc
        { obj_id = _
        ; nsamples
        ; is_major = _
        ; single_allocation_size = _
        ; size = _
        ; backtrace
        } ->
      let backtrace =
        match direction with
        | Filter.Explore_downwards_from_allocations -> backtrace
        | Explore_upwards_from_main -> List.rev backtrace
      in
      Loc_hitters.insert hhh backtrace nsamples
    | Promote _ -> ()
    | Collect _ -> ()
    | End -> ());
  hhh
;;

let bytes_of_samples ~rate ~word_size samples =
  let words = Float.of_int samples /. rate in
  Byte_units.scale word_size words
;;

let trie_of_hhh ~loc_cache ~rate ~word_size ~frequency hhh =
  let total_samples = Loc_hitters.total hhh in
  let threshold =
    Float.to_int (Float.round_down (frequency *. Float.of_int total_samples))
  in
  (* Convert the node if it's bigger than the threshold, and report its total samples *)
  let rec convert_node loc node : (Data.Location.t * Data.Trie.Node.t) option * int =
    let samples_excluding_children = Loc_hitters.Node.samples_excluding_children node in
    let samples = ref samples_excluding_children in
    let children =
      List.filter_map (Loc_hitters.Node.children node) ~f:(fun (loc, child) ->
        let converted, child_samples = convert_node loc child in
        samples := !samples + child_samples;
        converted)
      |> Data.Location.Map.of_alist_exn
    in
    let samples = !samples in
    let node_opt =
      let delta = Loc_hitters.Node.delta node in
      let location = Location.Cache.get_data loc_cache loc in
      if samples + delta >= threshold
      then (
        let allocations = bytes_of_samples ~rate ~word_size samples in
        let allocations_excluding_children =
          bytes_of_samples ~rate ~word_size samples_excluding_children
        in
        let on_unroll =
          if samples_excluding_children + delta >= threshold
          then Data.Entry.On_unroll.Keep
          else Hide
        in
        let entry =
          Data.Entry.create ~allocations ~allocations_excluding_children ~on_unroll
        in
        Some (location, Data.Trie.Node.create ~entry ~children))
      else None
    in
    node_opt, samples
  in
  let roots =
    List.filter_map (Loc_hitters.roots hhh) ~f:(fun (loc, root) ->
      fst (convert_node loc root))
    |> Data.Location.Map.of_alist_exn
  in
  let total_allocations = bytes_of_samples ~rate ~word_size total_samples in
  Data.Trie.create ~roots ~total_allocations
;;

let build ~trace ~loc_cache ~error ~frequency ~direction =
  let tinfo = Trace.Reader.info (Filtered_trace.trace trace) in
  let rate = tinfo.sample_rate in
  let word_size = tinfo.word_size / 8 |> Byte_units.of_bytes_int in
  let hhh = count ~trace ~error ~direction in
  trie_of_hhh ~loc_cache ~rate ~word_size ~frequency hhh
;;
