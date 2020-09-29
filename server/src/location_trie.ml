open! Core_kernel
open Memtrace
open Memtrace_viewer_common

module Location_code = struct
  module T = struct
    type t = Trace.Location_code.t

    let hash t = Int.hash (t : t :> int)
    let hash_fold_t s t = Int.hash_fold_t s (t : t :> int)
    let compare t1 t2 = Int.compare (t1 : t :> int) (t2 : t :> int)
    let sexp_of_t t = Int.sexp_of_t (t : t :> int)
  end

  include T
  include Hashable.Make_plain (T)
end

let convert_loc (loc : Trace.Location.t) =
  Data.Location.create
    ~filename:loc.filename
    ~defname:loc.defname
    ~line:loc.line
    ~start_char:loc.start_char
    ~end_char:loc.end_char
;;

module Location : sig
  type t

  val first : t
  val next : t -> t

  include Hashable.S with type t := t
end = struct
  include Int

  let first = 0
  let next x = x + 1
end

module Location_cache : sig
  type t

  val create : trace:Trace.Reader.t -> unit -> t
  val locs_from_code : t -> Location_code.t -> Location.t list
  val get_data : t -> Location.t -> Data.Location.t
end = struct
  type t =
    { trace : Trace.Reader.t
    ; mutable next_location : Location.t
    ; code_table : Location.t list Location_code.Table.t
    ; data_table : Data.Location.t Location.Table.t
    ; location_table : Location.t Data.Location.Table.t
    }

  let create ~trace () =
    { trace
    ; next_location = Location.first
    ; code_table = Location_code.Table.create ()
    ; data_table = Location.Table.create ()
    ; location_table = Data.Location.Table.create ()
    }
  ;;

  let locs_from_code t loc_code : Location.t list =
    Location_code.Table.find_or_add t.code_table loc_code ~default:(fun () ->
      let locs = Trace.Reader.lookup_location_code t.trace loc_code in
      List.map locs ~f:(fun loc_data ->
        let loc_data = convert_loc loc_data in
        Data.Location.Table.find_or_add t.location_table loc_data ~default:(fun () ->
          let loc = t.next_location in
          t.next_location <- Location.next loc;
          Location.Table.add_exn t.data_table ~key:loc ~data:loc_data;
          loc)))
  ;;

  let get_data t loc : Data.Location.t = Location.Table.find_exn t.data_table loc
end

module Loc_hitters = Hierarchical_heavy_hitters.Make (Location)

let count ~trace ~loc_cache ~error ~direction =
  let hhh = Loc_hitters.create error in
  let seen = Location.Table.create () in
  Filtered_trace.iter trace (fun _time ev ->
    match ev with
    | Event
        (Alloc
           { obj_id = _
           ; length = _
           ; nsamples
           ; is_major = _
           ; backtrace_buffer
           ; backtrace_length
           ; common_prefix = _
           }) ->
      let trace = ref [] in
      Location.Table.clear seen;
      (* Note that [backtrace_buffer] is in inverted order (main first), so [trace] ends
         up uninverted (allocation first). *)
      for i = 0 to backtrace_length - 1 do
        let loc_code = backtrace_buffer.(i) in
        let locs = Location_cache.locs_from_code loc_cache loc_code in
        List.iter locs ~f:(fun loc ->
          if not (Location.Table.mem seen loc)
          then (
            trace := loc :: !trace;
            Location.Table.add_exn seen ~key:loc ~data:()))
      done;
      let trace =
        match direction with
        | Filter.Explore_downwards_from_allocations -> !trace
        | Explore_upwards_from_main -> List.rev !trace
      in
      Loc_hitters.insert hhh trace nsamples
    | Event (Promote _) -> ()
    | Event (Collect _) -> ()
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
      let location = Location_cache.get_data loc_cache loc in
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

let build ~trace ~error ~frequency ~direction =
  let loc_cache = Location_cache.create ~trace:(Filtered_trace.trace trace) () in
  let tinfo = Trace.Reader.info (Filtered_trace.trace trace) in
  let rate = tinfo.sample_rate in
  let word_size = tinfo.word_size / 8 |> Byte_units.of_bytes_int in
  let hhh = count ~trace ~loc_cache ~error ~direction in
  trie_of_hhh ~loc_cache ~rate ~word_size ~frequency hhh
;;
