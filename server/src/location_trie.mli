open! Core_kernel
open Memtrace_viewer_common

val build
  :  trace:Filtered_trace.t
  -> loc_cache:Location.Cache.t
  -> error:float
  -> frequency:float
  -> direction:Filter.direction
  -> Data.Trie.t
