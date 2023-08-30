open! Core
open Memtrace_viewer_common

module Fragment_queue : sig
  type t

  val create : unit -> t
  val add_exn : t -> Data.Fragment.t -> unit
  val pop_max_allocations : t -> Data.Fragment.t option
  val remove : t -> Data.Fragment.t -> unit
end = struct
  module Item = struct
    type t =
      { fragment : Data.Fragment.t
      ; allocations : Byte_units.t
      }

    let of_fragment fragment =
      let allocations = Data.Entry.allocations (Data.Fragment.entry fragment) in
      { fragment; allocations }
    ;;

    let compare t1 t2 =
      (* Higher total count = higher priority (thus < because min heap) *)
      Byte_units.compare t2.allocations t1.allocations
    ;;
  end

  module Id = Data.Fragment.Id

  type t =
    { queue : Item.t Pairing_heap.t
    ; elts : Item.t Pairing_heap.Elt.t Id.Table.t
    }

  let create () =
    let queue = Pairing_heap.create ~cmp:Item.compare () in
    let elts = Id.Table.create () in
    { queue; elts }
  ;;

  let add_exn { queue; elts } fragment =
    let elt = Pairing_heap.add_removable queue (fragment |> Item.of_fragment) in
    Hashtbl.add_exn elts ~key:(Data.Fragment.id fragment) ~data:elt
  ;;

  let pop_max_allocations { queue; elts } =
    let max = Pairing_heap.pop queue in
    Option.map max ~f:(fun { fragment; allocations = _ } ->
      Hashtbl.remove elts (Data.Fragment.id fragment);
      fragment)
  ;;

  let remove { queue; elts } fragment =
    let id = Data.Fragment.id fragment in
    let elt = Hashtbl.find_and_remove elts id in
    Option.iter ~f:(Pairing_heap.remove queue) elt
  ;;
end

let hot_paths trie =
  let queue = Fragment_queue.create () in
  let rec enqueue_all_representatives node =
    if (not (Data.Fragment.is_empty node))
       && Data.Fragment.same node (Data.Fragment.representative node)
    then Fragment_queue.add_exn queue node;
    List.iter
      (Data.Fragment.one_frame_extensions node ~orient:Callees)
      ~f:(fun (_, child) -> enqueue_all_representatives child)
  in
  enqueue_all_representatives (Data.Fragment_trie.empty_fragment trie);
  let rec loop ~hot_paths =
    match Fragment_queue.pop_max_allocations queue with
    | None -> hot_paths
    | Some node ->
      let hot_paths = node :: hot_paths in
      let rec remove_descendents node ~orient =
        List.iter ~f:(remove ~orient) (Data.Fragment.one_frame_extensions ~orient node)
      and remove (_, node) ~orient =
        Fragment_queue.remove queue (Data.Fragment.representative node);
        remove_descendents node ~orient
      in
      remove_descendents node ~orient:Callers;
      remove_descendents node ~orient:Callees;
      loop ~hot_paths
  in
  let hot_paths = loop ~hot_paths:[] in
  List.rev hot_paths
;;
