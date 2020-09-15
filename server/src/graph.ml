open! Core_kernel
open Memtrace_viewer_common

type obj_info = { size : int }


let build ~trace ~size =
  (* Naively divide up the trace into equal segments to get [size] points *)
  let n =
    let counter = ref 0 in
    Filtered_trace.iter ~parse_backtraces:false trace (fun _ _ -> counter := !counter + 1);
    !counter
  in
  let objects : obj_info Obj_id.Table.t = Obj_id.Table.create () in
  let total_size = ref Int63.zero in
  let samples : (Time_ns.Span.t * Byte_units.t) list ref = ref [] in
  (* Take a sample at index 0 and every (n-1)/(size-1) entries thereafter; since
     the sample index is incremented size-1 times, the last index is therefore at
     most n-1 *)
  let sample_increment = (n - 1) / (size - 1) in
  let sample_increment = max sample_increment 1 in
  let index = ref 0 in
  Filtered_trace.iter trace (fun time event ->
    (match event with
     | Alloc { obj_id; nsamples; _ } ->
       Obj_id.Table.add_exn objects ~key:obj_id ~data:{ size = nsamples };
       total_size := Int63.(!total_size + (nsamples |> of_int))
     | Promote _ -> ()
     | Collect obj_id ->
       let obj_info = Obj_id.Table.find_exn objects obj_id in
       Obj_id.Table.remove objects obj_id;
       total_size := Int63.(!total_size - (obj_info.size |> of_int)));
    if !index % sample_increment = 0
    then
      samples
      := ( time |> Filtered_trace.time_span_of_timedelta
         , !total_size |> Byte_units.of_bytes_int63 )
         :: !samples;
    index := !index + 1);
  Data.Graph.create (List.rev !samples)
;;
