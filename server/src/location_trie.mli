open! Core_kernel
open Memtrace_viewer_common

val build
  :  trace:Filtered_trace.t
  -> error:float
  -> frequency:float
  -> direction:Filter.direction
  -> Data.Trie.t
