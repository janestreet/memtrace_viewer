open! Core
open Bonsai_web_proc

type 'a t =
  { value : 'a
  ; view : Vdom.Node.t
  }

val value : 'a t -> 'a
val view : _ t -> Vdom.Node.t
val map : f:('a -> 'b) -> 'a t -> 'b t
val map_view : f:(Vdom.Node.t -> Vdom.Node.t) -> 'a t -> 'a t
