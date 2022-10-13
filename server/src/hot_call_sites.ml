open! Core
open Memtrace_viewer_common.Data

(* A fragment creates an ordering on the locations that appear within it. The location of
   the first callee is lowest in the ordering, then the location of the second callee,
   etc. *)
let rec less_than_fragment_callees fragment location1 location2 =
  let first = Fragment.first fragment ~orient:Callees in
  if Location.equal first location1
  then true
  else if Location.equal first location2
  then false
  else (
    match Fragment.retract fragment ~orient:Callees with
    | None -> failwith "Hot_locations.less_than_callee"
    | Some next -> less_than_fragment_callees next location1 location2)
;;

let hot_locations trie =
  let tbl = Fragment.Id.Table.create () in
  let () =
    Fragment_trie.fold_singletons trie ~init:() ~f:(fun ~location ~fragment () ->
      if not (Location.is_special location)
      then (
        let rep = Fragment.representative fragment in
        let rep_id = Fragment.id rep in
        Hashtbl.update tbl rep_id ~f:(function
          | None -> location, fragment
          | Some ((old_location, _) as old_data) ->
            if less_than_fragment_callees rep location old_location
            then location, fragment
            else old_data)))
  in
  let unsorted =
    Hashtbl.fold tbl ~init:[] ~f:(fun ~key:_ ~data:(_, fragment) acc -> fragment :: acc)
  in
  List.sort unsorted ~compare:(fun a b ->
    Byte_units.compare
      (Entry.allocations (Fragment.entry b))
      (Entry.allocations (Fragment.entry a)))
;;
