open! Core

type 'a t = (Obj_id.t, 'a) Hashtbl.t

let create () = Obj_id.Table.create ()
let add_exn = Hashtbl.add_exn
let find_and_remove_exn t key = Hashtbl.find_and_remove t key |> Option.value_exn
