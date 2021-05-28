open! Core_kernel
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

let convert_loc (loc : Memtrace.Trace.Location.t) =
  Data.Location.create
    ~filename:loc.filename
    ~defname:loc.defname
    ~line:loc.line
    ~start_char:loc.start_char
    ~end_char:loc.end_char
;;

module T : sig
  include Identifier.S

  val allocation_site : t
  val toplevel : t
  val dummy : t
end = struct
  include Identifier.Make ()

  let allocation_site = first_special
  let toplevel = next_special allocation_site
  let dummy = max_value
end

module Cache = struct
  type t =
    { trace : Memtrace.Trace.Reader.t
    ; loc_gen : T.Generator.t
    ; code_table : T.t list Code.Table.t
    ; data_table : Data.Location.t T.Table.t
    ; location_table : T.t Data.Location.Table.t
    }

  let dummy_data =
    Data.Location.create
      ~filename:"(none)"
      ~defname:"(none)"
      ~start_char:0
      ~end_char:0
      ~line:0
  ;;

  let create ~trace () =
    let data_table = T.Table.create () in
    T.Table.add_exn data_table ~key:T.allocation_site ~data:Data.Location.allocation_site;
    T.Table.add_exn data_table ~key:T.toplevel ~data:Data.Location.toplevel;
    T.Table.add_exn data_table ~key:T.dummy ~data:dummy_data;
    { trace
    ; loc_gen = T.Generator.create ()
    ; code_table = Code.Table.create ()
    ; data_table
    ; location_table = Data.Location.Table.create ()
    }
  ;;

  let loc_from_data t loc_data =
    Data.Location.Table.find_or_add t.location_table loc_data ~default:(fun () ->
      let loc = T.Generator.generate t.loc_gen in
      assert (T.(loc < dummy));
      T.Table.add_exn t.data_table ~key:loc ~data:loc_data;
      loc)
  ;;

  let locs_from_code t loc_code : T.t list =
    Code.Table.find_or_add t.code_table loc_code ~default:(fun () ->
      let locs = Memtrace.Trace.Reader.lookup_location_code t.trace loc_code in
      List.map locs ~f:(fun loc_data ->
        let loc_data = convert_loc loc_data in
        loc_from_data t loc_data))
  ;;

  let get_data t loc : Data.Location.t = T.Table.find_exn t.data_table loc
end

include T
