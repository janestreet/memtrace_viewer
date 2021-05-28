open! Core_kernel
open! Async
open Memtrace_viewer_common

let percent x = x /. 100.

(* Allocation size (as percentage of total) below which a node is pruned from the trie
   returned to the client. *)
let default_significance_frequency = 0.5 |> percent

(* Upper bound on measurement errors (used in the substring heavy hitters algorithm). *)
let default_tolerance = 0.01 |> percent

(* Number of points in the time series produced for the graph. *)
let graph_size = 450

let time_ns_of_memtrace_timestamp ts =
  Int64.(1000L * (ts |> Memtrace.Trace.Timestamp.to_int64))
  |> Int63.of_int64_exn
  |> Time_ns.of_int63_ns_since_epoch
;;

let info_of_trace_info
      { Memtrace.Trace.Info.sample_rate
      ; word_size
      ; executable_name
      ; host_name
      ; ocaml_runtime_params
      ; pid
      ; start_time
      ; context
      }
  =
  { Data.Info.sample_rate
  ; word_size
  ; executable_name
  ; host_name
  ; ocaml_runtime_params
  ; pid
  ; start_time = start_time |> time_ns_of_memtrace_timestamp
  ; context
  }
;;

module Initial = struct
  type t =
    { trace : Memtrace.Trace.Reader.t
    ; loc_cache : Location.Cache.t
    ; graph : Data.Graph.t
    ; trie : Data.Fragment_trie.t
    ; info : Data.Info.t
    }

  let of_trace trace =
    let loc_cache = Location.Cache.create ~trace () in
    let filtered_trace = Filtered_trace.create ~trace ~loc_cache ~filter:Filter.default in
    let graph = Graph.build ~trace:filtered_trace ~size:graph_size in
    let trie =
      Location_trie.build
        ~trace:filtered_trace
        ~loc_cache
        ~tolerance:default_tolerance
        ~significance_frequency:default_significance_frequency
    in
    let info = info_of_trace_info (Memtrace.Trace.Reader.info trace) in
    { trace; loc_cache; graph; trie; info }
  ;;
end

type t =
  { mutable data : Data.t
  ; mutable filter : Filter.t
  }
[@@deriving fields]

let compute ~initial_state:Initial.{ trace; loc_cache; trie; graph; info } ~filter =
  let total_allocations_unfiltered = Data.Fragment_trie.total_allocations trie in
  let trie, filtered_graph =
    if Filter.is_default filter
    then trie, None
    else (
      let filtered_trace = Filtered_trace.create ~trace ~loc_cache ~filter in
      let trie =
        Location_trie.build
          ~trace:filtered_trace
          ~loc_cache
          ~tolerance:default_tolerance
          ~significance_frequency:default_significance_frequency
      in
      let filtered_graph = Graph.build ~trace:filtered_trace ~size:graph_size in
      trie, Some filtered_graph)
  in
  let hot_paths = Hot_paths.hot_paths trie in
  let info = Some info in
  { Data.graph; filtered_graph; trie; total_allocations_unfiltered; hot_paths; info }
;;

let create ~initial_state ~filter =
  let data = compute ~initial_state ~filter in
  { data; filter }
;;

let reset initial_state t =
  let filter = Filter.default in
  t.filter <- filter;
  let data = compute ~initial_state ~filter in
  t.filter <- filter;
  t.data <- data
;;

let update initial_state t action =
  match action with
  | Action.Set_filter filter ->
    t.filter <- filter;
    let data = compute ~initial_state ~filter in
    t.data <- data
;;
