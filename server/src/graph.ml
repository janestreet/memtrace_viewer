open! Core_kernel
open Memtrace_viewer_common

type obj_info = { size : Byte_units.t }

let full_graph_and_max_time ~trace : (Time_ns.Span.t * Byte_units.t) list * Time_ns.Span.t
  =
  let objects : obj_info Obj_id.Table.t = Obj_id.Table.create () in
  let total_size = ref Byte_units.zero in
  let points : (Time_ns.Span.t * Byte_units.t) list ref = ref [] in
  let max_time = ref Time_ns.Span.zero in
  Filtered_trace.iter trace (fun time event ->
    (match event with
     | Event (Alloc { obj_id; nsamples; _ }) ->
       let size = nsamples |> Filtered_trace.bytes_of_nsamples ~trace in
       Obj_id.Table.add_exn objects ~key:obj_id ~data:{ size };
       total_size := Byte_units.(!total_size + size)
     | Event (Promote _) -> ()
     | Event (Collect obj_id) ->
       let obj_info = Obj_id.Table.find_exn objects obj_id in
       Obj_id.Table.remove objects obj_id;
       total_size := Byte_units.(!total_size - obj_info.size)
     | End -> max_time := time);
    points := (time, !total_size) :: !points);
  List.rev !points, !max_time
;;

let take_samples ~full_graph ~max_time ~count : (Time_ns.Span.t * Byte_units.t) list =
  let count_float = count |> Float.of_int in
  let sample_time ~index =
    let frac_done = (index |> Float.of_int) /. count_float in
    Time_ns.Span.scale max_time frac_done
  in
  let rec sample ~index ~total_size ~points =
    if index > count
    then (
      assert (List.is_empty points);
      [])
    else (
      let sample_time = sample_time ~index in
      let window, points =
        List.split_while points ~f:(fun (time, _) -> Time_ns.Span.(time <= sample_time))
      in
      let total_size =
        match List.last window with
        | Some (_, total_size) -> total_size
        | None -> total_size
      in
      let index = index + 1 in
      (sample_time, total_size) :: sample ~index ~total_size ~points)
  in
  let samples = sample ~index:1 ~points:full_graph ~total_size:Byte_units.zero in
  assert (List.length samples = count);
  samples
;;

let build ~trace ~size =
  let full_graph, max_time = full_graph_and_max_time ~trace in
  let samples = take_samples ~max_time ~count:size ~full_graph in
  Data.Graph.create samples
;;
