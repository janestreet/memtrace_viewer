open! Core_kernel
open Memtrace_viewer_common


let predicate_matches ~loc_cache (pred : Filter.Location_predicate.t) loc =
  let data = Location.Cache.get_data loc_cache loc in
  match pred with
  | Defname_related { relation; rhs } ->
    let defname = Data.Location.defname data in
    (match relation with
     | Equals -> String.equal defname rhs
     | Contains -> String.is_substring defname ~substring:rhs)
;;

module Filtered_location_cache = struct
  type t =
    | Trivial of Location.Cache.t
    | Nontrivial of
        { loc_cache : Location.Cache.t
        ; hidden_locations : Filter.Location_predicate.t list
        ; cache : Location.t list Location.Code.Table.t
        }

  let create ~filter:({ hidden_locations; _ } : Filter.t) ~loc_cache () =
    match hidden_locations with
    | [] -> Trivial loc_cache
    | _ ->
      Nontrivial { loc_cache; hidden_locations; cache = Location.Code.Table.create () }
  ;;

  let locs_from_code t code =
    match t with
    | Trivial loc_cache -> Location.Cache.locs_from_code loc_cache code
    | Nontrivial { loc_cache; hidden_locations; cache } ->
      Location.Code.Table.find_or_add cache code ~default:(fun () ->
        let locs = Location.Cache.locs_from_code loc_cache code in
        List.filter locs ~f:(fun loc ->
          not
            (List.exists hidden_locations ~f:(fun pred ->
               predicate_matches ~loc_cache pred loc))))
  ;;
end

let time_span_of_timedelta time =
  let us = time |> Memtrace.Trace.Timedelta.to_int64 in
  Int64.(us * 1000L) |> Int63.of_int64_exn |> Time_ns.Span.of_int63_ns
;;

let bytes_of_int_words ~trace words =
  let info = Memtrace.Trace.Reader.info trace in
  words * info.word_size |> Byte_units.of_bytes_int
;;

let bytes_of_nsamples ~trace nsamples =
  let info = Memtrace.Trace.Reader.info trace in
  let words = Float.of_int nsamples /. info.sample_rate in
  words *. (info.word_size |> Float.of_int) |> Byte_units.of_bytes_float_exn
;;

module Event = struct
  type t =
    | Alloc of
        { obj_id : Obj_id.t
        ; is_major : bool
        ; single_allocation_size : Byte_units.t
        ; nsamples : int
        ; size : Byte_units.t
        ;
          backtrace : Location.t list
        }
    | Promote of Obj_id.t
    | Collect of Obj_id.t
    | End

  let of_memtrace_event ~trace ~loc_cache (event : Memtrace.Trace.Event.t) =
    match event with
    | Alloc
        { obj_id
        ; is_major
        ; length
        ; nsamples
        ; backtrace_buffer
        ; backtrace_length
        ; common_prefix = _
        } ->
      let single_allocation_size = length |> bytes_of_int_words ~trace in
      let size = nsamples |> bytes_of_nsamples ~trace in
      let backtrace =
        let trace = ref [] in
        let seen = Location.Table.create () in
        (* Note that [backtrace_buffer] is in inverted order (main first), so [trace] ends
           up uninverted (allocation first). *)
        for i = 0 to backtrace_length - 1 do
          let loc_code = backtrace_buffer.(i) in
          let locs = Filtered_location_cache.locs_from_code loc_cache loc_code in
          List.iter locs ~f:(fun loc ->
            if not (Location.Table.mem seen loc)
            then (
              trace := loc :: !trace;
              Location.Table.add_exn seen ~key:loc ~data:()))
        done;
        !trace
      in
      Alloc { obj_id; is_major; single_allocation_size; nsamples; size; backtrace }
    | Promote obj_id -> Promote obj_id
    | Collect obj_id -> Collect obj_id
  ;;
end

module Cached_predicate : sig
  type t

  val wrap : loc_cache:Location.Cache.t -> Filter.Location_predicate.t -> t
  val matches : t -> Location.t -> bool
end = struct
  type t =
    { pred : Filter.Location_predicate.t
    ; cache : bool Location.Table.t
    ; loc_cache : Location.Cache.t
    }

  let wrap ~loc_cache pred = { pred; cache = Location.Table.create (); loc_cache }

  let matches { pred; cache; loc_cache } loc =
    Location.Table.find_or_add cache loc ~default:(fun () ->
      predicate_matches ~loc_cache pred loc)
  ;;
end

(** Is an object allocated at this time eligible to pass the filter? *)
let should_record_allocation_at time ({ allocated_range; _ } : Filter.t) =
  Range.Time_ns_span.compare_point time allocated_range = 0
;;

(** Is an allocation with the given size eligible to pass the filter? *)
let should_record_allocation_of_size size ({ size_range; _ } : Filter.t) =
  Range.Byte_units.compare_point size size_range = 0
;;

module Location_filterer : sig
  type t

  val create : filter:Filter.t -> loc_cache:Location.Cache.t -> unit -> t
  val always_passes : t -> bool
  val should_record_allocation_with_backtrace : t -> Location.Code.t array -> int -> bool
end = struct
  type t =
    { required_locations : Cached_predicate.t list
    ; forbidden_locations : Cached_predicate.t list
    ; loc_cache : Location.Cache.t
    ; always_passes : bool
    }

  let create ~(filter : Filter.t) ~loc_cache () =
    let always_passes =
      match filter with
      | { required_locations = []
        ; forbidden_locations = []
        ; hidden_locations = _
        ; allocated_range = _
        ; collected_range = _
        ; size_range = _
        ; direction = _
        ; include_major_heap = _
        ; include_minor_heap = _
        } -> true
      | _ -> false
    in
    let required_locations =
      List.map ~f:(Cached_predicate.wrap ~loc_cache) filter.required_locations
    in
    let forbidden_locations =
      List.map ~f:(Cached_predicate.wrap ~loc_cache) filter.forbidden_locations
    in
    { required_locations; forbidden_locations; loc_cache; always_passes }
  ;;

  let always_passes t = t.always_passes

  (** Is an allocation with the given backtrace eligible to pass the filter? *)
  let should_record_allocation_with_backtrace t buffer length =
    let holds_somewhere pred =
      let rec holds_somewhere_from i =
        if i >= length
        then false
        else (
          let locs = Location.Cache.locs_from_code t.loc_cache buffer.(i) in
          List.exists ~f:(Cached_predicate.matches pred) locs
          || holds_somewhere_from (i + 1))
      in
      holds_somewhere_from 0
    in
    let interesting = List.for_all ~f:holds_somewhere t.required_locations in
    let forbidden () = List.exists ~f:holds_somewhere t.forbidden_locations in
    interesting && not (forbidden ())
  ;;
end

(** Is an object collected at this time eligible to pass the filter? *)
let should_keep_object_collected_at time ({ collected_range; _ } : Filter.t) =
  Range.Time_ns_span.Or_empty.contains_point collected_range time
;;

(** Are objects that are never collected eligible to pass the filter? (This counts objects
    that are live at the end as collected at the end, unless the filter specifies "never
    collected". In other words, "collected at or after" includes "collected never". This
    is important because "live at t" is interpreted as "allocated at or before t and
    collected after t". *)
let should_keep_objects_that_are_never_collected
      ~time_at_end
      ({ collected_range; _ } : Filter.t)
  =
  Range.Time_ns_span.Or_empty.(
    is_empty collected_range || contains_point collected_range time_at_end)
;;

let obj_ids_matching_filter ~trace ~loc_cache (filter : Filter.t) =
  assert (filter.include_minor_heap || filter.include_major_heap);
  (* Objects that are live in an interesting heap *)
  let live = Obj_id.Hash_set.create () in
  (* Objects that will become live if promoted *)
  let prelive = Obj_id.Hash_set.create () in
  let passing = Obj_id.Hash_set.create () in
  let last_time = ref Time_ns.Span.zero in
  let location_filterer = Location_filterer.create ~filter ~loc_cache () in
  let filtering_by_backtrace = not (Location_filterer.always_passes location_filterer) in
  let parse_backtraces = filtering_by_backtrace in
  Memtrace.Trace.Reader.iter ~parse_backtraces trace (fun time event ->
    let time = time |> time_span_of_timedelta in
    let defer obj_id = Hash_set.strict_add_exn prelive obj_id in
    let is_deferred obj_id = Hash_set.mem prelive obj_id in
    let allocate obj_id =
      Hash_set.remove prelive obj_id;
      if should_record_allocation_at time filter
      then Hash_set.strict_add_exn live obj_id
    in
    let deallocate obj_id =
      if Hash_set.mem prelive obj_id
      then (
        Hash_set.strict_remove_exn prelive obj_id;
        assert (not (Hash_set.mem live obj_id)))
      else if Hash_set.mem live obj_id
      then (
        Hash_set.strict_remove_exn live obj_id;
        if should_keep_object_collected_at time filter
        then Hash_set.strict_add_exn passing obj_id)
    in
    last_time := time;
    match event with
    | Alloc { obj_id; length; is_major; backtrace_length; backtrace_buffer; _ } ->
      let deferring = (not is_major) && not filter.include_minor_heap in
      let definitely_wrong_heap = is_major && not filter.include_major_heap in
      let correct_size =
        should_record_allocation_of_size (length |> bytes_of_int_words ~trace) filter
      in
      let interesting_backtrace () =
        (not filtering_by_backtrace)
        || Location_filterer.should_record_allocation_with_backtrace
             location_filterer
             backtrace_buffer
             backtrace_length
      in
      let eligible =
        (not definitely_wrong_heap) && correct_size && interesting_backtrace ()
      in
      if not eligible then () else if deferring then defer obj_id else allocate obj_id
    | Promote obj_id ->
      if is_deferred obj_id
      then allocate obj_id
      else if not filter.include_major_heap
      then deallocate obj_id
    | Collect obj_id -> deallocate obj_id
    (* Might already have been filtered out, but [deallocate] is harmless then *));
  let time_at_end = !last_time in
  if should_keep_objects_that_are_never_collected filter ~time_at_end
  then Hash_set.iter live ~f:(fun obj_id -> Hash_set.strict_add_exn passing obj_id);
  passing
;;

type t =
  { trace : Memtrace.Trace.Reader.t
  ; loc_cache : Filtered_location_cache.t
  ; interesting : Obj_id.t -> bool
  ; defer_minor_allocations : bool
  ; collect_on_promotion : bool
  }

let trace { trace; _ } = trace

let create ~trace ~loc_cache ~filter =
  let interesting =
    if Filter.is_always_true filter
    then fun _ -> true
    else (
      let interesting = obj_ids_matching_filter ~trace ~loc_cache filter in
      fun obj_id -> Hash_set.mem interesting obj_id)
  in
  let filtered_loc_cache = Filtered_location_cache.create ~filter ~loc_cache () in
  let defer_minor_allocations = not filter.include_minor_heap in
  let collect_on_promotion = not filter.include_major_heap in
  { trace
  ; loc_cache = filtered_loc_cache
  ; interesting
  ; defer_minor_allocations
  ; collect_on_promotion
  }
;;

let iter
      { trace; loc_cache; interesting; defer_minor_allocations; collect_on_promotion }
      ?parse_backtraces
      f
  =
  let deferring = Obj_id.Table.create () in
  let collected_early = Obj_id.Hash_set.create () in
  let last_time = ref Time_ns.Span.zero in
  Memtrace.Trace.Reader.iter trace ?parse_backtraces (fun time event ->
    let time = time |> time_span_of_timedelta in
    last_time := time;
    match event with
    | (Alloc { obj_id; _ } | Promote obj_id | Collect obj_id)
      when not (interesting obj_id) -> ()
    | Alloc ({ obj_id; is_major = false; _ } as alloc) when defer_minor_allocations ->
      Obj_id.Table.add_exn
        deferring
        ~key:obj_id
        ~data:(Memtrace.Trace.Event.Alloc { alloc with is_major = true })
    | Promote obj_id when collect_on_promotion ->
      Hash_set.strict_add_exn collected_early obj_id;
      f time (Event.Collect obj_id)
    | Promote obj_id when defer_minor_allocations ->
      (match Obj_id.Table.find_and_remove deferring obj_id with
       | None ->
         raise
           (Not_found_s
              [%message "Missing deferred object" ~obj_id:((obj_id :> int) : int)])
       | Some event -> f time (event |> Event.of_memtrace_event ~trace ~loc_cache))
    | Collect obj_id when collect_on_promotion && Hash_set.mem collected_early obj_id ->
      Hash_set.remove collected_early obj_id
    | _ -> f time (event |> Event.of_memtrace_event ~trace ~loc_cache));
  f !last_time Event.End
;;
