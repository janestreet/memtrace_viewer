open! Core

type 'a t

val create : unit -> _ t
val add_exn : 'a t -> key:Obj_id.t -> data:'a -> unit
val find_and_remove_exn : 'a t -> Obj_id.t -> 'a
