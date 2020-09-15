open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

let format_loc_in_file loc =
  let filename = Data.Location.filename loc in
  let line = Data.Location.line loc in
  let start_char = Data.Location.start_char loc in
  let end_char = Data.Location.end_char loc in
  sprintf "(%s:%d:%d-%d)" filename line start_char end_char
;;

let format_string loc =
  let defname = Data.Location.defname loc in
  sprintf "%s %s" defname (format_loc_in_file loc)
;;

let format_dom loc =
  let defname = Data.Location.defname loc in
  Vdom.Node.span
    [ Vdom.Attr.class_ "loc" ]
    [ Vdom.Node.textf "%s " defname
    ; Vdom.Node.span
        [ Vdom.Attr.class_ "loc-in-file" ]
        [ Vdom.Node.text (format_loc_in_file loc) ]
    ]
;;
