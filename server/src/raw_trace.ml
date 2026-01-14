open! Core
open Memtrace_viewer_common

type t =
  { trace : Memtrace.Trace.Reader.t
  ; info : Data.Info.t
  }

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
  ; initial_domain = _
  ; start_time
  ; context
  }
  =
  { Data.Info.sample_rate
  ; word_size = word_size / 8 |> Byte_units.of_bytes_int
  ; executable_name
  ; host_name
  ; ocaml_runtime_params
  ; pid
  ; start_time = start_time |> time_ns_of_memtrace_timestamp
  ; context
  }
;;

let time_span_of_timedelta time =
  let us = time |> Memtrace.Trace.Timedelta.to_int64 in
  Int64.(us * 1000L) |> Int63.of_int64_exn |> Time_ns.Span.of_int63_ns
;;

let info t = t.info
let bytes_of_words t words = Byte_units.scale t.info.word_size words
let bytes_of_int_words t words = bytes_of_words t (words |> Float.of_int)

let bytes_of_nsamples t nsamples =
  let words = Float.of_int nsamples /. t.info.sample_rate in
  bytes_of_words t words
;;

module Event = struct
  type t = Location.Code.t Event.t [@@deriving sexp_of]

  let of_memtrace_event trace (event : Memtrace.Trace.Event.t) : t =
    match event with
    | Alloc
        { obj_id
        ; length
        ; nsamples
        ; source
        ; backtrace_buffer
        ; backtrace_length
        ; common_prefix
        ; domain
        } ->
      let single_allocation_size = length |> bytes_of_int_words trace in
      let size = nsamples |> bytes_of_nsamples trace in
      Alloc
        { obj_id
        ; source
        ; single_allocation_size
        ; nsamples
        ; size
        ; backtrace_buffer
        ; backtrace_length
        ; common_prefix
        ; domain
        }
    | Promote (obj_id, _) -> Promote obj_id
    | Collect (obj_id, _) -> Collect obj_id
  ;;
end

let of_memtrace_trace trace =
  let info = Memtrace.Trace.Reader.info trace |> info_of_trace_info in
  { trace; info }
;;

let iter t ~parse_backtraces f =
  let last_time = ref Time_ns.Span.zero in
  Memtrace.Trace.Reader.iter t.trace ~parse_backtraces (fun time event ->
    let time = time |> time_span_of_timedelta in
    let event = event |> Event.of_memtrace_event t in
    last_time := time;
    f time event);
  f !last_time End
;;
