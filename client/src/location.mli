open! Core
open Bonsai_web.Proc
open Memtrace_viewer_common

val format_dom : call_sites:Data.Call_site.t list option -> Data.Location.t -> Vdom.Node.t
