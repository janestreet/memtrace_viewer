open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

let render_loc loc = Vdom.Node.li [] [ Location.format_dom loc ]

let render ~(direction : Filter.direction) backtrace =
  let locs =
    match direction with
    | Explore_downwards_from_allocations -> backtrace
    | Explore_upwards_from_main -> List.rev backtrace
  in
  let items = List.map ~f:render_loc locs in
  Vdom.Node.ul [ Vdom.Attr.class_ "backtrace" ] items
;;
