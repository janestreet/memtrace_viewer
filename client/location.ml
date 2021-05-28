open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

(* Add a soft line break before each period. Without this, the anchor panel usually wants
   to be very wide. *)
let with_soft_line_breaks text =
  String.concat_map text ~f:(function
    | '.' -> "\u{200b}." (* Unicode zero-width space *)
    | c -> c |> String.of_char)
;;

let format_string loc =
  let defname = Data.Location.defname loc in
  if Data.Location.is_special loc
  then defname
  else
    sprintf
      "%s %s"
      (defname |> with_soft_line_breaks)
      (Data.Location.loc_in_file_to_string loc)
;;

let format_dom loc =
  let defname = Data.Location.defname loc in
  if Data.Location.is_special loc
  then
    Vdom.Node.span
      [ Vdom.Attr.classes [ "loc"; "loc-special" ] ]
      [ Vdom.Node.text defname ]
  else
    Vdom.Node.span
      [ Vdom.Attr.class_ "loc" ]
      [ Vdom.Node.textf "%s " (defname |> with_soft_line_breaks)
      ; Vdom.Node.span
          [ Vdom.Attr.class_ "loc-in-file" ]
          [ Vdom.Node.text (Data.Location.loc_in_file_to_string loc) ]
      ]
;;
