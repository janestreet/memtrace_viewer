open! Core
open Bonsai_web
open Memtrace_viewer_common

let render_loc loc = Vdom.Node.li [ Location.format_dom loc ]

let trivial_backtrace =
  let open Vdom in
  Node.p
    ~attr:(Attr.classes [ "backtrace"; "backtrace-empty"; "indented" ])
    [ Node.text "(none)" ]
;;

let render backtrace =
  let open Vdom in
  match backtrace with
  | [] -> trivial_backtrace
  | _ when Data.Backtrace.is_trivial backtrace -> trivial_backtrace
  (* Note that display order is reversed *)
  | last_frame :: rest as backtrace ->
    (* Rather than showing the special frames, indicate them by the /lack/ of an ellipsis
    *)
    let open_below, backtrace =
      if Data.Location.is_toplevel last_frame then false, rest else true, backtrace
    in
    let open_above, backtrace =
      match List.rev backtrace with
      | [] ->
        (* This must be just the toplevel *)
        assert (not open_below);
        true, []
      | first_frame :: rest as backtrace ->
        if Data.Location.is_allocation_site first_frame
        then false, rest
        else true, backtrace
    in
    let ellipsis_if cond =
      if cond
      then
        Some
          (Node.li
             ~attr:(Attr.class_ "backtrace-ellipsis")
             [ Node.text "\u{22ee}" (* VERTICAL ELLIPSIS *) ])
      else None
    in
    let last_item, backtrace =
      (* Note that [backtrace] is still reversed from above *)
      match backtrace with
      | [] -> None, []
      | last_item :: backtrace -> Some last_item, backtrace
    in
    let first_item, backtrace =
      match List.rev backtrace with
      | [] -> None, backtrace
      | first_item :: backtrace -> Some first_item, backtrace
    in
    let skipping = not (List.is_empty backtrace) in
    let items =
      List.filter_opt
        [ ellipsis_if open_above
        ; Option.map ~f:render_loc first_item
        ; ellipsis_if skipping
        ; Option.map ~f:render_loc last_item
        ; ellipsis_if open_below
        ]
    in
    Node.ul ~attr:(Attr.class_ "backtrace") items
;;
