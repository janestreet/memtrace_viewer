open! Core_kernel
open Memtrace_viewer_common

val build : trace:Filtered_trace.t -> size:int -> Data.Graph.t
