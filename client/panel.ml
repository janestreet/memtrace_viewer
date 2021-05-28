open! Core_kernel
open Bonsai_web

let panel ?title body ~id =
  let open Vdom in
  let body =
    match title with
    | None -> [ body ]
    | Some title -> [ Node.h2 [ Attr.class_ "panel_title" ] [ Node.text title ]; body ]
  in
  Node.section
    [ Attr.class_ "panel"; Attr.id id ]
    [ Node.div [ Attr.class_ "panel-content" ] body ]
;;
