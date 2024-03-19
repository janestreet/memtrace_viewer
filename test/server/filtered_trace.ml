open! Core
open Memtrace_viewer_common
open Memtrace_viewer_native.For_testing

let memtrace_trace = Memtrace.Trace.Reader.open_ ~filename:"test_trace.ctf"
let trace = memtrace_trace |> Raw_trace.of_memtrace_trace

let loc_cache =
  (* Keep the same location cache between tests so that the locations are consistent *)
  Location.Cache.create () ~trace:memtrace_trace
;;

module Location_with_defname = struct
  type t = Location.t

  let plain_name defname =
    match String.rsplit2 defname ~on:'.' with
    | None -> "<init>"
    | Some (_, name) -> name
  ;;

  let sexp_of_t t =
    let str =
      match Location.Cache.get_loc_data loc_cache t with
      | Function f -> Data.Function.defname f |> plain_name
      | Allocation_site s -> Data.Allocation_site.short_name s
      | loc -> Data.Location.defname loc
    in
    Sexp.Atom str
  ;;
end

module Obj_id = struct
  type t = Memtrace.Trace.Obj_id.t

  let sexp_of_t (t : t) = sexp_of_int (t :> int)
end

module Source = struct
  type t = Memtrace.Trace.Allocation_source.t =
    | Minor
    | Major
    | External
  [@@deriving sexp_of]
end

module Dumped_event_format = struct
  type t =
    | Alloc of Obj_id.t * Source.t * Byte_units.t * Location_with_defname.t array
    | Promote of Obj_id.t
    | Collect of Obj_id.t
    | End
  [@@deriving sexp_of]

  let of_event (event : Filtered_trace.Event.t) =
    match event with
    | Alloc { obj_id; source; size; backtrace_buffer; backtrace_length; _ } ->
      let backtrace = Array.sub backtrace_buffer ~pos:0 ~len:backtrace_length in
      Alloc (obj_id, source, size, backtrace)
    | Promote obj_id -> Promote obj_id
    | Collect obj_id -> Collect obj_id
    | End -> End
  ;;
end

module Dumped_event = struct
  type t = Filtered_trace.Event.t

  let sexp_of_t (t : t) = Dumped_event_format.sexp_of_t (t |> Dumped_event_format.of_event)
end

let dump_events ?limit (filtered_trace : Filtered_trace.t) =
  let exception Done in
  let count = ref 0 in
  try
    Filtered_trace.iter filtered_trace ~mode:Preserve_backtraces (fun time event ->
      print_s [%message "" (time : Time_ns.Span.t) (event : Dumped_event.t)];
      incr count;
      match limit with
      | Some limit when limit <= !count -> raise_notrace Done
      | _ -> ())
  with
  | Done -> ()
;;

let run_dump_test ?limit () ~filter =
  let filtered_trace = Filtered_trace.create ~trace ~filter ~loc_cache in
  dump_events ?limit filtered_trace
;;

let%expect_test "large allocs" =
  run_dump_test
    ()
    ~filter:
      { Filter.always_true with
        size_range = Range.Byte_units.range (Open (Byte_units.of_kilobytes 1.0)) No_bound
      };
  [%expect
    {|
    ((time 1.036ms)
     (event
      (Alloc 36 Major 7.34375K (<init> main go b a c create array0.ml:40:6-18))))
    ((time 1.847ms) (event (Collect 36)))
    ((time 1.847ms) (event End))
    |}]
;;

let%expect_test "short lifetimes" =
  run_dump_test
    ()
    ~filter:
      { Filter.always_true with
        lifetime_range =
          Range.Time_ns_span.range No_bound (Open (Time_ns.Span.of_int_us 5))
      };
  [%expect
    {|
    ((time 1.034ms)
     (event
      (Alloc 27 Minor 80B (<init> main go c b a generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 30 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.035ms)
     (event
      (Alloc 34 Minor 240B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.038ms) (event (Collect 27)))
    ((time 1.038ms) (event (Collect 30)))
    ((time 1.039ms) (event (Collect 34)))
    ((time 1.121ms)
     (event
      (Alloc 37 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.122ms)
     (event
      (Alloc 39 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.123ms)
     (event
      (Alloc 42 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.123ms)
     (event
      (Alloc 44 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.123ms)
     (event
      (Alloc 45 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.123ms)
     (event
      (Alloc 46 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.124ms)
     (event
      (Alloc 48 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.125ms)
     (event
      (Alloc 51 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.125ms) (event (Collect 37)))
    ((time 1.125ms) (event (Collect 39)))
    ((time 1.126ms) (event (Collect 42)))
    ((time 1.126ms) (event (Collect 44)))
    ((time 1.126ms) (event (Collect 45)))
    ((time 1.126ms) (event (Collect 46)))
    ((time 1.126ms) (event (Collect 48)))
    ((time 1.126ms) (event (Collect 51)))
    ((time 1.847ms) (event End))
    |}]
;;

let%expect_test "only array functions" =
  run_dump_test
    ()
    ~filter:
      { Filter.always_true with
        required_locations = [ Defname_related { relation = Contains; rhs = "Array" } ]
      };
  [%expect
    {|
    ((time 895us)
     (event (Alloc 0 Minor 160B (<init> main go create array0.ml:40:6-18))))
    ((time 1.036ms)
     (event
      (Alloc 36 Major 7.34375K (<init> main go b a c create array0.ml:40:6-18))))
    ((time 1.036ms) (event (Promote 0)))
    ((time 1.847ms) (event (Collect 36)))
    ((time 1.847ms) (event End))
    |}]
;;

let%expect_test "no b" =
  run_dump_test
    ()
    ~filter:
      { Filter.always_true with
        forbidden_locations = [ Defname_related { relation = Contains; rhs = ".b" } ]
      };
  [%expect
    {|
    ((time 895us)
     (event (Alloc 0 Minor 160B (<init> main go create array0.ml:40:6-18))))
    ((time 1.02ms)
     (event
      (Alloc 7 Minor 160B (<init> main go a generate_test_data.ml:20:22-44))))
    ((time 1.036ms) (event (Promote 0)))
    ((time 1.037ms) (event (Promote 7)))
    ((time 1.847ms) (event End))
    |}]
;;

let%expect_test "hide b" =
  run_dump_test
    ()
    ~limit:10
    ~filter:
      { Filter.always_true with
        hidden_locations = [ Defname_related { relation = Contains; rhs = ".b" } ]
      };
  [%expect
    {|
    ((time 895us)
     (event (Alloc 0 Minor 160B (<init> main go create array0.ml:40:6-18))))
    ((time 1.015ms)
     (event
      (Alloc 1 Minor 80B (<init> main go a c generate_test_data.ml:20:22-44))))
    ((time 1.017ms)
     (event
      (Alloc 2 Minor 80B (<init> main go a c generate_test_data.ml:20:22-44))))
    ((time 1.017ms) (event (Alloc 3 Minor 160B (<init> main go a c))))
    ((time 1.018ms) (event (Alloc 4 Minor 80B (<init> main go a c))))
    ((time 1.018ms) (event (Alloc 5 Minor 160B (<init> main go a c))))
    ((time 1.019ms)
     (event
      (Alloc 6 Minor 80B (<init> main go c a generate_test_data.ml:20:22-44))))
    ((time 1.02ms)
     (event
      (Alloc 7 Minor 160B (<init> main go a generate_test_data.ml:20:22-44))))
    ((time 1.02ms) (event (Alloc 8 Minor 160B (<init> main go a c))))
    ((time 1.02ms)
     (event
      (Alloc 9 Minor 80B (<init> main go a c generate_test_data.ml:20:22-44))))
    |}]
;;

let%expect_test "show major heap only" =
  run_dump_test
    ()
    ~limit:25
    ~filter:{ Filter.always_true with include_minor_heap = false };
  [%expect
    {|
    ((time 895us)
     (event (Alloc 0 Minor 160B (<init> main go create array0.ml:40:6-18))))
    ((time 1.015ms)
     (event
      (Alloc 1 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.018ms)
     (event
      (Alloc 4 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.018ms)
     (event
      (Alloc 5 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.019ms)
     (event
      (Alloc 6 Minor 80B (<init> main go b c a generate_test_data.ml:20:22-44))))
    ((time 1.02ms)
     (event
      (Alloc 7 Minor 160B (<init> main go a generate_test_data.ml:20:22-44))))
    ((time 1.02ms)
     (event
      (Alloc 9 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.022ms)
     (event
      (Alloc 14 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.022ms)
     (event
      (Alloc 15 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.023ms)
     (event
      (Alloc 21 Minor 80B (<init> main go b a generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 22 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 23 Minor 160B (<init> main go c b a generate_test_data.ml:34:22-57))))
    ((time 1.033ms)
     (event
      (Alloc 25 Minor 160B (<init> main go a c b generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 28 Minor 80B (<init> main go c b a generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 29 Minor 80B (<init> main go a b generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 31 Minor 240B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.035ms)
     (event
      (Alloc 32 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.035ms)
     (event
      (Alloc 33 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.035ms)
     (event
      (Alloc 35 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.036ms)
     (event
      (Alloc 36 Major 7.34375K (<init> main go b a c create array0.ml:40:6-18))))
    ((time 1.036ms) (event (Promote 0)))
    ((time 1.036ms) (event (Promote 1)))
    ((time 1.037ms) (event (Promote 4)))
    ((time 1.037ms) (event (Promote 5)))
    ((time 1.037ms) (event (Promote 6)))
    |}]
;;

let%expect_test "all events" =
  run_dump_test () ~filter:Filter.always_true;
  [%expect
    {|
    ((time 895us)
     (event (Alloc 0 Minor 160B (<init> main go create array0.ml:40:6-18))))
    ((time 1.015ms)
     (event
      (Alloc 1 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.017ms)
     (event
      (Alloc 2 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.017ms)
     (event
      (Alloc 3 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.018ms)
     (event
      (Alloc 4 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.018ms)
     (event
      (Alloc 5 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.019ms)
     (event
      (Alloc 6 Minor 80B (<init> main go b c a generate_test_data.ml:20:22-44))))
    ((time 1.02ms)
     (event
      (Alloc 7 Minor 160B (<init> main go a generate_test_data.ml:20:22-44))))
    ((time 1.02ms)
     (event
      (Alloc 8 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.02ms)
     (event
      (Alloc 9 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.021ms)
     (event
      (Alloc 10 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.021ms)
     (event
      (Alloc 11 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.022ms)
     (event
      (Alloc 12 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.022ms)
     (event
      (Alloc 13 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.022ms)
     (event
      (Alloc 14 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.022ms)
     (event
      (Alloc 15 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.022ms)
     (event
      (Alloc 16 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.022ms)
     (event
      (Alloc 17 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 18 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 19 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 20 Minor 80B (<init> main go b a generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 21 Minor 80B (<init> main go b a generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 22 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.023ms)
     (event
      (Alloc 23 Minor 160B (<init> main go c b a generate_test_data.ml:34:22-57))))
    ((time 1.033ms)
     (event
      (Alloc 24 Minor 80B (<init> main go a c b generate_test_data.ml:20:22-44))))
    ((time 1.033ms)
     (event
      (Alloc 25 Minor 160B (<init> main go a c b generate_test_data.ml:20:22-44))))
    ((time 1.033ms)
     (event
      (Alloc 26 Minor 80B (<init> main go c b a generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 27 Minor 80B (<init> main go c b a generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 28 Minor 80B (<init> main go c b a generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 29 Minor 80B (<init> main go a b generate_test_data.ml:20:22-44))))
    ((time 1.034ms)
     (event
      (Alloc 30 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.034ms)
     (event
      (Alloc 31 Minor 240B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.035ms)
     (event
      (Alloc 32 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.035ms)
     (event
      (Alloc 33 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.035ms)
     (event
      (Alloc 34 Minor 240B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.035ms)
     (event
      (Alloc 35 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.036ms)
     (event
      (Alloc 36 Major 7.34375K (<init> main go b a c create array0.ml:40:6-18))))
    ((time 1.036ms) (event (Promote 0)))
    ((time 1.036ms) (event (Promote 1)))
    ((time 1.036ms) (event (Collect 2)))
    ((time 1.037ms) (event (Collect 3)))
    ((time 1.037ms) (event (Promote 4)))
    ((time 1.037ms) (event (Promote 5)))
    ((time 1.037ms) (event (Promote 6)))
    ((time 1.037ms) (event (Promote 7)))
    ((time 1.037ms) (event (Collect 8)))
    ((time 1.037ms) (event (Promote 9)))
    ((time 1.037ms) (event (Collect 10)))
    ((time 1.037ms) (event (Collect 11)))
    ((time 1.037ms) (event (Collect 12)))
    ((time 1.037ms) (event (Collect 13)))
    ((time 1.037ms) (event (Promote 14)))
    ((time 1.038ms) (event (Promote 15)))
    ((time 1.038ms) (event (Collect 16)))
    ((time 1.038ms) (event (Collect 17)))
    ((time 1.038ms) (event (Collect 18)))
    ((time 1.038ms) (event (Collect 19)))
    ((time 1.038ms) (event (Collect 20)))
    ((time 1.038ms) (event (Promote 21)))
    ((time 1.038ms) (event (Promote 22)))
    ((time 1.038ms) (event (Promote 23)))
    ((time 1.038ms) (event (Collect 24)))
    ((time 1.038ms) (event (Promote 25)))
    ((time 1.038ms) (event (Collect 26)))
    ((time 1.038ms) (event (Collect 27)))
    ((time 1.038ms) (event (Promote 28)))
    ((time 1.038ms) (event (Promote 29)))
    ((time 1.038ms) (event (Collect 30)))
    ((time 1.039ms) (event (Promote 31)))
    ((time 1.039ms) (event (Promote 32)))
    ((time 1.039ms) (event (Promote 33)))
    ((time 1.039ms) (event (Collect 34)))
    ((time 1.039ms) (event (Promote 35)))
    ((time 1.121ms)
     (event
      (Alloc 37 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.122ms)
     (event
      (Alloc 38 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.122ms)
     (event
      (Alloc 39 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.122ms)
     (event
      (Alloc 40 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.122ms)
     (event
      (Alloc 41 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.123ms)
     (event
      (Alloc 42 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.123ms)
     (event
      (Alloc 43 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.123ms)
     (event
      (Alloc 44 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.123ms)
     (event
      (Alloc 45 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.123ms)
     (event
      (Alloc 46 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.124ms)
     (event
      (Alloc 47 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.124ms)
     (event
      (Alloc 48 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.124ms)
     (event
      (Alloc 49 Minor 80B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.124ms)
     (event
      (Alloc 50 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.125ms)
     (event
      (Alloc 51 Minor 160B (<init> main go b a c generate_test_data.ml:34:22-57))))
    ((time 1.125ms)
     (event
      (Alloc 52 Minor 160B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.125ms)
     (event
      (Alloc 53 Minor 80B (<init> main go b a c generate_test_data.ml:20:22-44))))
    ((time 1.125ms) (event (Collect 37)))
    ((time 1.125ms) (event (Promote 38)))
    ((time 1.125ms) (event (Collect 39)))
    ((time 1.125ms) (event (Promote 40)))
    ((time 1.125ms) (event (Promote 41)))
    ((time 1.126ms) (event (Collect 42)))
    ((time 1.126ms) (event (Promote 43)))
    ((time 1.126ms) (event (Collect 44)))
    ((time 1.126ms) (event (Collect 45)))
    ((time 1.126ms) (event (Collect 46)))
    ((time 1.126ms) (event (Promote 47)))
    ((time 1.126ms) (event (Collect 48)))
    ((time 1.126ms) (event (Promote 49)))
    ((time 1.126ms) (event (Promote 50)))
    ((time 1.126ms) (event (Collect 51)))
    ((time 1.126ms) (event (Promote 52)))
    ((time 1.126ms) (event (Promote 53)))
    ((time 1.847ms) (event (Collect 36)))
    ((time 1.847ms) (event End))
    |}]
;;
