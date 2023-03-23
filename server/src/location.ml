open! Core
open Memtrace_viewer_common

module Code = struct
  module T = struct
    type t = Memtrace.Trace.Location_code.t

    let hash t = Int.hash (t : t :> int)
    let hash_fold_t s t = Int.hash_fold_t s (t : t :> int)
    let compare t1 t2 = Int.compare (t1 : t :> int) (t2 : t :> int)
    let sexp_of_t t = Int.sexp_of_t (t : t :> int)
  end

  include T
  include Hashable.Make_plain (T)
end

module Trace_location = struct
  module T = struct
    type t = Memtrace.Trace.Location.t =
      { filename : string
      ; line : int
      ; start_char : int
      ; end_char : int
      ; defname : string
      }
    [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)

  let to_call_site t =
    Data.Call_site.create
      ~filename:t.filename
      ~line:t.line
      ~start_char:t.start_char
      ~end_char:t.end_char
      ~defname:t.defname
  ;;
end

module T : sig
  include Identifier.S

  val allocator : t
  val toplevel : t
  val dummy : t
end = struct
  include Identifier.Make ()

  let allocator = first_special
  let toplevel = next_special allocator
  let dummy = max_value
end

module Cache = struct
  module Call_site_entry = struct
    type t =
      { func : T.t
      ; data : Data.Call_site.t
      }
  end

  type t =
    { trace : Memtrace.Trace.Reader.t
    ; loc_gen : T.Generator.t
    ; call_site_gen : Call_site.Generator.t
    ; code_table : Call_site.t list Code.Table.t
    ; call_site_data_table : Call_site_entry.t Call_site.Table.t
    ; call_sites_by_trace_loc : Call_site.t Trace_location.Table.t
    ; loc_data_table : Data.Location.t T.Table.t
    ; functions_by_defname : T.t String.Table.t
    ; allocation_sites_by_call_site : T.t Call_site.Table.t
    }

  let create ~trace () =
    let loc_data_table : Data.Location.t T.Table.t = T.Table.create () in
    Hashtbl.add_exn loc_data_table ~key:T.allocator ~data:Data.Location.allocator;
    Hashtbl.add_exn loc_data_table ~key:T.toplevel ~data:Data.Location.toplevel;
    Hashtbl.add_exn loc_data_table ~key:T.dummy ~data:Data.Location.dummy;
    { trace
    ; loc_gen = T.Generator.create ()
    ; call_site_gen = Call_site.Generator.create ()
    ; code_table = Code.Table.create ()
    ; call_site_data_table = Call_site.Table.create ()
    ; call_sites_by_trace_loc = Trace_location.Table.create ()
    ; loc_data_table
    ; functions_by_defname = String.Table.create ()
    ; allocation_sites_by_call_site = Call_site.Table.create ()
    }
  ;;

  let function_from_defname t defname =
    Hashtbl.find_or_add t.functions_by_defname defname ~default:(fun () ->
      let loc = T.Generator.generate t.loc_gen in
      assert (T.(loc < dummy));
      let entry = Data.Location.create_function (Data.Function.create ~defname) in
      Hashtbl.add_exn t.loc_data_table ~key:loc ~data:entry;
      loc)
  ;;

  let call_site_from_trace_location t tloc =
    Hashtbl.find_or_add t.call_sites_by_trace_loc tloc ~default:(fun () ->
      let call_site = Call_site.Generator.generate t.call_site_gen in
      let func = function_from_defname t tloc.defname in
      let data = Trace_location.to_call_site tloc in
      let entry : Call_site_entry.t = { func; data } in
      Hashtbl.add_exn t.call_site_data_table ~key:call_site ~data:entry;
      call_site)
  ;;

  let call_sites_from_code t loc_code : Call_site.t list =
    Hashtbl.find_or_add t.code_table loc_code ~default:(fun () ->
      let call_sites = Memtrace.Trace.Reader.lookup_location_code t.trace loc_code in
      List.map call_sites ~f:(fun call_site -> call_site_from_trace_location t call_site))
  ;;

  let get_defname t loc : string =
    let data = Hashtbl.find_exn t.loc_data_table loc in
    Data.Location.defname data
  ;;

  let call_site_entry t call_site : Call_site_entry.t =
    Hashtbl.find_exn t.call_site_data_table call_site
  ;;

  let get_call_site_data t call_site : Data.Call_site.t =
    (call_site_entry t call_site).data
  ;;

  let get_function_of_call_site t call_site : T.t = (call_site_entry t call_site).func

  let get_allocation_site_of_call_site t call_site : T.t =
    Hashtbl.find_or_add t.allocation_sites_by_call_site call_site ~default:(fun () ->
      let loc = T.Generator.generate t.loc_gen in
      let data = get_call_site_data t call_site in
      let entry = Data.Location.create_allocation_site data in
      Hashtbl.add_exn t.loc_data_table ~key:loc ~data:entry;
      loc)
  ;;

  let get_loc_data t loc : Data.Location.t = Hashtbl.find_exn t.loc_data_table loc
end

include T
