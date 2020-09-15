open! Core_kernel
open Memtrace_viewer_common
open Bonsai_web

val render : direction:Filter.direction -> Data.Backtrace.t -> Vdom.Node.t
