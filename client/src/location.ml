open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

let allocation_site_indicator =
  let open Vdom in
  Node.span
    ~attrs:[ Attr.class_ "loc-alloc-site-indicator"; Attr.title "Allocation site" ]
    [ Node.text "A" ]
;;

let format_dom ~call_sites loc =
  let open Vdom in
  let defname = Data.Location.defname loc in
  let call_sites = call_sites |> Option.value ~default:[] in
  let special text =
    Node.span ~attrs:[ Attr.classes [ "loc"; "loc-special" ] ] [ Node.text text ]
  in
  match loc with
  | Allocation_site alloc_site ->
    let filename = Data.Allocation_site.filename alloc_site in
    let position = Data.Allocation_site.position alloc_site in
    Node.span
      ~attrs:[ Attr.class_ "loc" ]
      [ allocation_site_indicator
      ; Node.textf "%s " defname
      ; Node.span
          ~attrs:[ Attr.class_ "call-sites" ]
          [ Node.textf "(%s:%s)" filename position ]
      ]
  | Function _ ->
    if List.is_empty call_sites
    then Node.span ~attrs:[ Attr.class_ "loc" ] [ Node.text defname ]
    else (
      let filename = Data.Call_site.filename (List.hd_exn call_sites) in
      let positions =
        List.map call_sites ~f:(fun call_site ->
          Node.span [ Node.text (Data.Call_site.position call_site) ])
        |> List.intersperse ~sep:(Node.text ", ")
      in
      let call_sites =
        Node.span
          ~attrs:[ Attr.class_ "call-sites" ]
          (List.concat [ [ Node.textf "(%s:" filename ]; positions; [ Node.text ")" ] ])
      in
      Node.span ~attrs:[ Attr.class_ "loc" ] [ Node.textf "%s " defname; call_sites ])
  | Allocator -> special "Allocation sites"
  | Toplevel -> special "OCaml toplevel"
  | Dummy -> special "(none)"
;;
