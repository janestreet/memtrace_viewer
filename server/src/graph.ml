open! Core
open Memtrace_viewer_common

type obj_info = { size : Byte_units.t }

let full_graph_and_max_time ~trace : (Time_ns.Span.t * Byte_units.t) list * Time_ns.Span.t
  =
  let all_events = ref [] in
  Filtered_trace.iter trace ~mode:Preserve_times (fun time event ->
    all_events := (time, event) :: !all_events);
  let all_events_ordered =
    List.sort
      ~compare:(fun (t1, _) (t2, _) -> Time_ns.Span.compare t1 t2)
      (List.rev !all_events)
  in
  let total_size = ref Byte_units.zero in
  let objects : obj_info Obj_id.Table.t = Obj_id.Table.create () in
  let max_time = ref Time_ns.Span.zero in
  let points =
    List.map
      ~f:(fun (time, event) ->
        (match event with
         | Alloc { obj_id; size; _ } ->
           Hashtbl.add_exn objects ~key:obj_id ~data:{ size };
           total_size := Byte_units.(!total_size + size)
         | Promote _ -> ()
         | Collect obj_id ->
           let obj_info = Hashtbl.find_exn objects obj_id in
           Hashtbl.remove objects obj_id;
           total_size := Byte_units.(!total_size - obj_info.size)
         | End -> max_time := time);
        time, !total_size)
      all_events_ordered
  in
  points, !max_time
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
