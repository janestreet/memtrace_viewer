open! Core
open Memtrace_viewer_common

type obj_info = { size : Byte_units.t } [@@unboxed]

let full_graph_and_max_time ~trace
  : (Time_ns.Span.t * Byte_units.t) iarray * Time_ns.Span.t
  =
  let all_events = Queue.create ~capacity:128 () in
  Filtered_trace.iter trace ~mode:Preserve_times (fun time event ->
    Queue.enqueue all_events (time, event));
  let all_events = Queue.to_array all_events in
  let () =
    (* In practice it appears the events are pre-sorted, and [Array.is_sorted] is *much*
       faster than [Array.sort] in this case, saving us a considerable amount of time on
       large traces. *)
    let[@inline always] compare (t1, _) (t2, _) = Time_ns.Span.compare t1 t2 in
    if not ((Array.is_sorted [@inlined hint]) all_events ~compare)
    then Array.sort all_events ~compare
  in
  let total_size = ref Byte_units.zero in
  let max_time = ref Time_ns.Span.zero in
  let points =
    let objects = Obj_id_table.create () in
    Array.map all_events ~f:(stack_ fun (time, event) ->
      (match event with
       | Alloc { obj_id; size; _ } ->
         Obj_id_table.add_exn objects ~key:obj_id ~data:{ size };
         total_size := Byte_units.(!total_size + size)
       | Promote _ -> ()
       | Collect obj_id ->
         let obj_info = Obj_id_table.find_and_remove_exn objects obj_id in
         total_size := Byte_units.(!total_size - obj_info.size)
       | End -> max_time := time);
      time, !total_size)
  in
  let points = Iarray.unsafe_of_array__promise_no_mutation points in
  points, !max_time
;;

let take_samples ~full_graph ~max_time ~count : (Time_ns.Span.t * Byte_units.t) Queue.t =
  let count_float = count |> Float.of_int in
  let sample_time ~index =
    let frac_done = (index |> Float.of_int) /. count_float in
    Time_ns.Span.scale max_time frac_done
  in
  let rec sample ~index ~total_size ~points ~offset ~samples =
    if index > count
    then assert (offset = Iarray.length points)
    else (
      let sample_time = sample_time ~index in
      let[@inline always] is_before_sample_time (time, _) =
        Time_ns.Span.O.(time <= sample_time)
      in
      let length = Iarray.length points in
      let starting_offset = offset in
      let offset = ref offset in
      while
        !offset < length && is_before_sample_time (Iarray.unsafe_get points !offset)
      do
        offset := !offset + 1
      done;
      let offset = !offset in
      let total_size =
        if offset > starting_offset
        then (
          let _, total_size' = Iarray.get points (offset - 1) in
          total_size')
        else total_size
      in
      let index = index + 1 in
      Queue.enqueue samples (sample_time, total_size);
      sample ~index ~total_size ~points ~offset ~samples)
  in
  let samples = Queue.create ~capacity:count () in
  sample ~index:1 ~points:full_graph ~total_size:Byte_units.zero ~offset:0 ~samples;
  assert (Queue.length samples = count);
  samples
;;

let build ~trace ~size =
  let full_graph, max_time = full_graph_and_max_time ~trace in
  let samples = take_samples ~max_time ~count:size ~full_graph in
  Data.Graph.create samples
;;
