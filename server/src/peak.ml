open! Core

(* This could probably all be done in [Filtered_trace], but there's enough going on in
   there already. *)

type t =
  { allocations : Byte_units.t
  ; time : Time_ns.Span.t
  }

let find_peak_allocations trace =
  let size_table = Obj_id.Table.create () in
  let current_total = ref Byte_units.zero in
  let peak_total = ref Byte_units.zero in
  let peak_time = ref Time_ns.Span.zero in
  Raw_trace.iter trace ~parse_backtraces:false (fun time event ->
    match event with
    | Alloc { obj_id; size; _ } ->
      Hashtbl.add_exn size_table ~key:obj_id ~data:size;
      (current_total := Byte_units.(!current_total + size));
      if Byte_units.(!current_total > !peak_total)
      then (
        peak_total := !current_total;
        peak_time := time)
    | Collect obj_id ->
      let size = Hashtbl.find_and_remove size_table obj_id |> Option.value_exn in
      current_total := Byte_units.(!current_total - size)
    | Promote _ -> ()
    | End -> ());
  { allocations = !peak_total; time = !peak_time }
;;
