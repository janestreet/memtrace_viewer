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
    | Alloc { obj_id; length; source; backtrace_length; backtrace_buffer; _ } ->
      let deferring =
        match source with
        | Minor -> not filter.include_minor_heap
        | Major | External -> false
      in
      let definitely_wrong_heap =
        match source with
        | Minor ->
          (* Could become interesting later (when promoted), so it's only possibly wrong
          *)
          false
        | Major -> not filter.include_major_heap
        | External ->
          not filter.include_major_heap
      in
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

module Event = struct
  type t =
    | Alloc of
        { obj_id : Obj_id.t
        ; source : Memtrace.Trace.Allocation_source.t
        ; single_allocation_size : Byte_units.t
        ; nsamples : int
        ; size : Byte_units.t
        ; backtrace_buffer : Location.t array
        ; backtrace_length : int
        ; common_prefix : int
        }
    | Promote of Obj_id.t
    | Collect of Obj_id.t
    | End
end

module Mode = struct
  type t =
    | Preserve_backtraces
    | Preserve_times
end

module Interpreter : sig
  type filtered_trace := t
  type t

  val create
    :  filtered_trace:filtered_trace
    -> callback:(Time_ns.Span.t -> Event.t -> unit)
    -> mode:Mode.t
    -> unit
    -> t

  val interpret_event : t -> Memtrace.Trace.Timedelta.t -> Memtrace.Trace.Event.t -> unit
  val done_ : t -> unit
end = struct
  type filtered_trace = t

  type t =
    { filtered_trace : filtered_trace
    ; callback : Time_ns.Span.t -> Event.t -> unit
    ; mode : Mode.t
    ; deferring : Memtrace.Trace.Event.t Obj_id.Table.t
    ; collected_early : Obj_id.Hash_set.t
    ; seen : Location.Hash_set.t
    ; mutable seen_at_each_in_index : Location.t list array
    ; mutable backtrace_buffer : Location.t array
    ; mutable out_lengths : int array
    ; mutable prev_in_length : int
    ; mutable prev_out_length : int
    ; mutable max_next_common_prefix : int
    ; mutable last_time : Time_ns.Span.t
    }

  let create ~filtered_trace ~callback ~mode () =
    { filtered_trace
    ; callback
    ; mode
    ; deferring = Obj_id.Table.create ()
    ; collected_early = Obj_id.Hash_set.create ()
    ; seen = Location.Hash_set.create ()
    ; seen_at_each_in_index = Array.create ~len:10 []
    ; backtrace_buffer = Array.create ~len:10 Location.dummy
    ; out_lengths = Array.create ~len:10 0
    ; prev_in_length = 0
    ; prev_out_length = 0
    ; max_next_common_prefix = Int.max_value
    ; last_time = Time_ns.Span.zero
    }
  ;;

  let ensure_capacity array ~index ~default =
    let len = index + 1 in
    let old_len = Array.length array in
    if old_len < len
    then (
      let new_len = max len (2 * old_len) in
      let new_array = Array.create ~len:new_len default in
      Array.blito ~src:array ~dst:new_array ();
      new_array)
    else array
  ;;

  (** The total number of out frames corresponding to the in frames in the interval
      {v [i,j) v}. See [move] for how this is used. *)
  let total_out_length t ~from_inc:i ~to_exc:j =
    let ans = ref 0 in
    for i = i to j - 1 do
      ans := !ans + t.out_lengths.(i)
    done;
    !ans
  ;;

  (** How much to adjust an out cursor if the in cursor changes from i to j.

      When we interpret an event, we have two buffers, one of which is a view of the other
      but with some elements skipped and some elements expanded. To track the common
      prefix, we have a cursor into each buffer:

      {v
        out lengths = [| 1 ; 0 ; 2     ; 3         ; 0 ; 0 ; 1 ; 0 ; 0 ; ... |]

                                                     in cursor
                                                         |
                                                         v
        in buffer   = [| a ; _ ; b     ; c         ; _ ; _ ; d ; _ ; _ ; ... |]

                                                     out cursor
                                                         |
                                                         v
        out buffer  = [| a ;     b ; b ; c ; c ; c ;         d ;             |]

      v}

      Here there's an underscore in the in buffer where an element is skipped
      ([out_lengths.(i) = 0) and repeated entries in the out buffer where an element is
      expanded ([out_lengths.(i) > 1]).

      Since the out buffer is a view on the in buffer, for consistency, the arrows must
      line up. Note that the out (bottom) cursor can be drawn anywhere within its "block",
      because moving the in cursor anywhere in that range only changes how many elements
      are skipped.

      Say we're moving the in cursor like so:

      {v
        out lengths = [| 1 ; 0 ; 2     ; 3         ; 0 ; 0 ; 1 ; 0 ; 0 ; ... |]

                         in cursor
                             | ---------------------------
                             v
        in buffer   = [| a ; _ ; b     ; c         ; _ ; _ ; d ; _ ; _ ; ... |]

                                                     out cursor
                                                         |
                                                         v
        out buffer  = [| a ;     b ; b ; c ; c ; c ;     d ;             |]
      v}

      We want to move the out cursor to the same place. Thus [move t i j = -5], since
      the out cursor needs to move left by five blocks.

      In general, to move from [i] to [j], we total the lengths in the half-open
      interval between them, then move that many spaces right if [i<j] or left if
      [i>j]. The interval is always inclusive at the lower end and exclusive at the
      higher end; if this seems arbitrary, we can redraw the above diagram with the
      cursors pointing at borders rather than elements:

      {v
        out lengths = [| 1 ; 0 ; 2     ; 3         ; 0 ; 0 ; 1 ; 0 ; 0 ; ... |]

                       in cursor = 1            old in cursor = 5
                           | ---------------------------
                           v
        in buffer   = [| a ; _ ; b     ; c         ; _ ; _ ; d ; _ ; _ ; ... |]

                                                   out cursor = 5
                                                       |
                                                       v
        out buffer  = [| a ;     b ; b ; c ; c ; c ;         d ;             |]
      v}

      Now it's clear that we want to count the element on the right of the cursor only on
      the low end. If we went from 5 to 0 instead, we would need to move the out cursor
      all the way to 0 (so the 1 at index 0 is counted). If we went from 5 to 6 instead,
      the out cursor would not move at all (so the 1 at index 6 is *not* counted).
  *)
  let move t ~from:i ~to_:j =
    let magnitude = total_out_length t ~from_inc:(min i j) ~to_exc:(max i j) in
    if i <= j then magnitude else ~-magnitude
  ;;

  let write_to_backtrace t ~index loc =
    (* We could use the [growable_array] library to guard this, but that doesn't let us
       get back the underlying array when producing an event. *)
    t.backtrace_buffer
    <- ensure_capacity t.backtrace_buffer ~index ~default:Location.dummy;
    t.backtrace_buffer.(index) <- loc
  ;;

  let write_out_length t ~index len =
    t.out_lengths <- ensure_capacity t.out_lengths ~index ~default:0;
    t.out_lengths.(index) <- len
  ;;

  let write_seen_at t ~index seen =
    t.seen_at_each_in_index <- ensure_capacity t.seen_at_each_in_index ~index ~default:[];
    t.seen_at_each_in_index.(index) <- seen
  ;;

  let conv_event (t : t) (event : Memtrace.Trace.Event.t) : Event.t =
    match event with
    | Alloc
        { obj_id
        ; source
        ; length
        ; nsamples
        ; backtrace_buffer = in_backtrace_buffer
        ; backtrace_length = in_backtrace_length
        ; common_prefix = in_common_prefix
        } ->
      let single_allocation_size =
        length |> bytes_of_int_words ~trace:t.filtered_trace.trace
      in
      let size = nsamples |> bytes_of_nsamples ~trace:t.filtered_trace.trace in
      let backtrace_length, common_prefix =
        match t.mode with
        | Preserve_times -> 0, 0
        | Preserve_backtraces ->
          let in_common_prefix = min in_common_prefix t.max_next_common_prefix in
          let out_common_prefix =
            if t.prev_out_length = 0
            then (* first backtrace *)
              0
            else
              (* Start from [t.prev_out_length - 1] to skip over the allocation site at the end
                 (slightly wasteful in the case that we get two consecutive identical backtraces
                 but that should be exceedingly rare) *)
              t.prev_out_length - 1 + move t ~from:t.prev_in_length ~to_:in_common_prefix
          in
          if in_common_prefix = 0
          then Hash_set.clear t.seen
          else
            for in_ix = in_common_prefix to t.prev_in_length - 1 do
              List.iter ~f:(Hash_set.remove t.seen) t.seen_at_each_in_index.(in_ix)
            done;
          let backtrace_length =
            let out_ix = ref out_common_prefix in
            let write loc =
              write_to_backtrace t ~index:!out_ix loc;
              incr out_ix
            in
            if !out_ix = 0 then write Location.toplevel;
            for in_ix = in_common_prefix to in_backtrace_length - 1 do
              let out_start = !out_ix in
              let loc_code = in_backtrace_buffer.(in_ix) in
              let locs =
                Filtered_location_cache.locs_from_code t.filtered_trace.loc_cache loc_code
              in
              let seen_here = ref [] in
              List.iter locs ~f:(fun loc ->
                if not (Hash_set.mem t.seen loc)
                then (
                  write loc;
                  Hash_set.add t.seen loc;
                  seen_here := loc :: !seen_here));
              write_seen_at t ~index:in_ix !seen_here;
              let out_length = !out_ix - out_start in
              write_out_length t ~index:in_ix out_length
            done;
            write Location.allocation_site;
            !out_ix
          in
          backtrace_length, out_common_prefix
      in
      let backtrace_buffer = t.backtrace_buffer in
      Alloc
        { obj_id
        ; source
        ; single_allocation_size
        ; nsamples
        ; size
        ; backtrace_buffer
        ; backtrace_length
        ; common_prefix
        }
    | Promote obj_id -> Promote obj_id
    | Collect obj_id -> Collect obj_id
  ;;

  let interpret_event t time (event : Memtrace.Trace.Event.t) =
    let { interesting; defer_minor_allocations; collect_on_promotion; _ } =
      t.filtered_trace
    in
    let time = time |> time_span_of_timedelta in
    t.last_time <- time;
    let return (out_event : Event.t) =
      let () =
        match t.mode with
        | Preserve_times -> ()
        | Preserve_backtraces ->
          (match event, out_event with
           | ( Alloc { backtrace_length = in_length; _ }
             , Alloc { backtrace_length = out_length; _ } ) ->
             t.prev_in_length <- in_length;
             t.prev_out_length <- out_length;
             t.max_next_common_prefix <- Int.max_value
           | Alloc _, _ | _, Alloc _ | _, End -> assert false
           | (Promote _ | Collect _), (Promote _ | Collect _) -> ())
      in
      t.callback time out_event
    in
    let skip () =
      (* If we don't pass the event through, we need to make sure the next common prefix
         is no larger than this one so that the next event will know to go back far enough
         to copy the backtrace from this event.
      *)
      let () =
        match t.mode with
        | Preserve_times -> ()
        | Preserve_backtraces ->
          (match event with
           | Alloc { common_prefix; _ } ->
             t.max_next_common_prefix <- min common_prefix t.max_next_common_prefix
           (* *Don't* update prev_in_length, since its purpose is to know how to move the
              out cursor *from the last event we interpreted* *)
           | Promote _ | Collect _ -> ())
      in
      ()
    in
    let interesting =
      let (Alloc { obj_id; _ } | Promote obj_id | Collect obj_id) = event in
      interesting obj_id
    in
    if not interesting
    then skip ()
    else (
      match t.mode with
      | Preserve_backtraces -> return (event |> conv_event t)
      | Preserve_times ->
        (match event with
         | Alloc ({ obj_id; source = Minor; _ } as alloc) when defer_minor_allocations ->
           Obj_id.Table.add_exn
             t.deferring
             ~key:obj_id
             ~data:(Alloc { alloc with source = Major });
           skip ()
         | Promote obj_id when collect_on_promotion ->
           Hash_set.strict_add_exn t.collected_early obj_id;
           return (Collect obj_id)
         | Promote obj_id when defer_minor_allocations ->
           (match Obj_id.Table.find_and_remove t.deferring obj_id with
            | None ->
              raise
                (Not_found_s
                   [%message "Missing deferred object" ~obj_id:((obj_id :> int) : int)])
            | Some event -> return (event |> conv_event t))
         | Collect obj_id
           when collect_on_promotion && Hash_set.mem t.collected_early obj_id ->
           Hash_set.remove t.collected_early obj_id
         | Collect obj_id
           when defer_minor_allocations && Obj_id.Table.mem t.deferring obj_id ->
           Obj_id.Table.remove t.deferring obj_id
         | _ -> return (event |> conv_event t)))
  ;;

  let done_ t = t.callback t.last_time End
end

let iter t ~mode f =
  let interpreter = Interpreter.create ~filtered_trace:t ~callback:f ~mode () in
  let parse_backtraces =
    match mode with
    | Mode.Preserve_backtraces -> true
    | Preserve_times -> false
  in
  Memtrace.Trace.Reader.iter t.trace ~parse_backtraces (fun time event ->
    Interpreter.interpret_event interpreter time event);
  Interpreter.done_ interpreter
;;
