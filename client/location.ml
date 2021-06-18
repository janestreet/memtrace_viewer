open! Core
open Bonsai_web
open Memtrace_viewer_common

let format_dom loc =
  let defname = Data.Location.defname loc in
  if Data.Location.is_special loc
  then
    Vdom.Node.span
      ~attr:(Vdom.Attr.classes [ "loc"; "loc-special" ])
      [ Vdom.Node.text defname ]
  else
    Vdom.Node.span
      ~attr:(Vdom.Attr.class_ "loc")
      [ Vdom.Node.textf "%s " defname
      ; Vdom.Node.span
          ~attr:(Vdom.Attr.class_ "loc-in-file")
          [ Vdom.Node.text (Data.Location.loc_in_file loc) ]
      ]
;;
