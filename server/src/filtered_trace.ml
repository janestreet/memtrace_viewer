open! Core_kernel
open Memtrace
open Memtrace_viewer_common

module Event = struct
  type t =
    | Event of Trace.Event.t
    | End
end

let time_span_of_timedelta time =
  let us = time |> Trace.Timedelta.to_int64 in
  Int64.(us * 1000L) |> Int63.of_int64_exn |> Time_ns.Span.of_int63_ns
;;

(** Is an object allocated at this time eligible to pass the filter? *)
let should_record_allocation_at time (ranges : Filter.Ranges.t) =
  Time_range.compare_point (time |> time_span_of_timedelta) ranges.allocated_range = 0
  && Time_range.compare_point (time |> time_span_of_timedelta) ranges.live_range <= 0
;;

(** Is an object collected at this time no longer eligible to pass the filter? *)
let should_forget_object_collected_at time (ranges : Filter.Ranges.t) =
  (* Only if we're still before the live range. At the start of the live range, everything
     we're tracking will definitely pass. *)
  Time_range.compare_point (time |> time_span_of_timedelta) ranges.live_range < 0
;;

let obj_ids_matching_filter
      ~trace
      { Filter.ranges; direction = _; include_minor_heap; include_major_heap }
  =
  let passing = Obj_id.Hash_set.create () in
  let deferred = Obj_id.Hash_set.create () in
  Trace.Reader.iter ~parse_backtraces:false trace (fun time event ->
    let allocate ~defer obj_id =
      if should_record_allocation_at time ranges
      then (
        Hash_set.strict_add_exn passing obj_id;
        if defer then Hash_set.strict_add_exn deferred obj_id)
    in
    let deallocate obj_id =
      if should_forget_object_collected_at time ranges
      then Hash_set.remove passing obj_id
    in
    match event with
    | Alloc { obj_id; is_major = true; _ } ->
      if include_major_heap then allocate ~defer:false obj_id
    | Alloc { obj_id; is_major = false; _ } ->
      if include_minor_heap then allocate ~defer:false obj_id
    | Promote obj_id ->
      (match include_major_heap, include_minor_heap with
       | true, false -> allocate ~defer:true obj_id
       | false, true -> deallocate obj_id
       | _ -> ())
    | Collect obj_id -> deallocate obj_id
    (* Might not have been allocated to begin with depending on which
       heap, but [Hash_set.remove] is harmless then *));
  passing, deferred
;;

type t =
  { trace : Trace.Reader.t
  ; interesting : Obj_id.t -> bool
  ; deferred : Obj_id.t -> bool
  ; collect_on_promotion : bool
  }

let trace { trace; _ } = trace

let create ~trace ~filter =
  let interesting, deferred =
    if Filter.always_true filter
    then (fun _ -> true), fun _ -> false
    else (
      let interesting, deferred = obj_ids_matching_filter ~trace filter in
      ( (fun obj_id -> Hash_set.mem interesting obj_id)
      , fun obj_id -> Hash_set.mem deferred obj_id ))
  in
  let collect_on_promotion = filter.include_minor_heap && not filter.include_major_heap in
  { trace; interesting; deferred; collect_on_promotion }
;;

let iter { trace; interesting; deferred; collect_on_promotion } ?parse_backtraces f =
  let deferring = Obj_id.Table.create () in
  let collected_early = Obj_id.Hash_set.create () in
  let last_time = ref Time_ns.Span.zero in
  Trace.Reader.iter trace ?parse_backtraces (fun time event ->
    let time = time |> time_span_of_timedelta in
    last_time := time;
    match event with
    | (Alloc { obj_id; _ } | Promote obj_id | Collect obj_id)
      when not (interesting obj_id) -> ()
    | Alloc ({ obj_id; _ } as alloc) when deferred obj_id ->
      Obj_id.Table.add_exn
        deferring
        ~key:obj_id
        ~data:(Trace.Event.Alloc { alloc with is_major = true })
    | Promote obj_id when collect_on_promotion ->
      Hash_set.strict_add_exn collected_early obj_id;
      f time (Event.Event (Collect obj_id))
    | Promote obj_id when deferred obj_id ->
      (match Obj_id.Table.find_and_remove deferring obj_id with
       | None ->
         raise
           (Not_found_s
              [%message "Missing deferred object" ~obj_id:((obj_id :> int) : int)])
       | Some event -> f time (Event event))
    | Collect obj_id when collect_on_promotion && Hash_set.mem collected_early obj_id ->
      Hash_set.remove collected_early obj_id
    | _ -> f time (Event event));
  f !last_time End
;;

let bytes_of_nsamples ~trace:{ trace; _ } nsamples =
  let { Memtrace.Trace.Info.sample_rate; word_size; _ } =
    Memtrace.Trace.Reader.info trace
  in
  let nwords = Float.of_int nsamples /. sample_rate in
  Float.of_int word_size *. nwords |> Byte_units.of_bytes_float_exn
;;
