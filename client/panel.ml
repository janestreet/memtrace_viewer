open! Core
open Bonsai_web

let panel ?title body ~id =
  let open Vdom in
  let body =
    match title with
    | None -> [ body ]
    | Some title ->
      [ Node.h2 ~attr:(Attr.class_ "panel_title") [ Node.text title ]; body ]
  in
  Node.section
    ~attr:(Attr.many_without_merge [ Attr.class_ "panel"; Attr.id id ])
    [ Node.div ~attr:(Attr.class_ "panel-content") body ]
;;
