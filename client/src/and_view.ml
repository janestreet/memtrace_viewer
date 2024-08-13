open! Core
open Bonsai_web.Proc

type 'a t =
  { value : 'a
  ; view : Vdom.Node.t
  }
[@@deriving fields ~getters]

let map ~f { value; view } = { value = f value; view }
let map_view ~f { value; view } = { value; view = f view }
