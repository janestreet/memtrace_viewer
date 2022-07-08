open! Core
open Memtrace_viewer_common

val build
  :  trace:Filtered_trace.t
  -> loc_cache:Location.Cache.t
  -> tolerance:float
  -> significance_frequency:float
  -> Data.Fragment_trie.t * Data.Call_sites.t
