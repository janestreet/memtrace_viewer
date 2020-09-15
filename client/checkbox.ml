open! Core_kernel
open Bonsai_web

type t =
  { view : Vdom.Node.t
  ; value : bool
  ; changing : bool
  ; reset_changing : Vdom.Event.t
  }
[@@deriving fields]

let component ~label ~initial_value =
  let open Bonsai.Let_syntax in
  let%sub changing = Changing.component (module Bool) ~initial_value in
  return
    (let%map changing = changing in
     let value = Changing.value changing in
     let set_value = Changing.set_value changing in
     let reset_changing = Changing.set_changing changing false in
     let changing = Changing.changing changing in
     let open Vdom in
     let view =
       let on_click _ = set_value (not value) in
       Node.label
         []
         [ Node.input
             (List.concat
                [ [ Attr.type_ "checkbox" ]
                ; [ Attr.on_click on_click ]
                ; (if value then [ Attr.checked ] else [])
                ])
             []
         ; Node.text label
         ]
     in
     { view; value; changing; reset_changing })
;;
