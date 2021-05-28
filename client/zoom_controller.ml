open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

let default_poi_backtrace = [ Data.Location.allocation_site ]

type t =
  { poi : Data.Fragment.t
  ; set_poi : Data.Fragment.t -> Vdom.Event.t
  ; zoom : Data.Fragment.t
  ; set_zoom : Data.Fragment.t option -> Vdom.Event.t
  }

let component ~trie =
  let open Bonsai.Let_syntax in
  let%sub poi_backtrace, set_poi_backtrace =
    Bonsai.state [%here] (module Data.Backtrace) ~default_model:default_poi_backtrace
  in
  let%sub zoom_backtrace, set_zoom_backtrace =
    Bonsai.state_opt [%here] (module Data.Backtrace)
  in
  return
    (let%mapn trie = trie
     and poi_backtrace = poi_backtrace
     and set_poi_backtrace = set_poi_backtrace
     and zoom_backtrace = zoom_backtrace
     and set_zoom_backtrace = set_zoom_backtrace in
     let poi =
       match Data.Fragment_trie.find trie poi_backtrace with
       | Some poi -> poi
       | None -> Data.Fragment_trie.find trie default_poi_backtrace |> Option.value_exn
     in
     let set_poi poi =
       Vdom.Event.Many
         [ set_poi_backtrace (Data.Fragment.backtrace poi); set_zoom_backtrace None ]
     in
     let zoom =
       let zoom = Option.bind ~f:(Data.Fragment_trie.find trie) zoom_backtrace in
       (* If [zoom] is None, either there's no zoom set or it's no longer valid *)
       zoom |> Option.value ~default:poi
     in
     let set_zoom zoom =
       set_zoom_backtrace (Option.map ~f:Data.Fragment.backtrace zoom)
     in
     { poi; set_poi; zoom; set_zoom })
;;
