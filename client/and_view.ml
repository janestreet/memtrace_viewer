open! Core
open Bonsai_web

type 'a t =
  { value : 'a
  ; view : Vdom.Node.t
  }
[@@deriving fields]

let map ~f { value; view } = { value = f value; view }
let map_view ~f { value; view } = { value; view = f view }
