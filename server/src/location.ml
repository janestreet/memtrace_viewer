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
  type t

  val first : t
  val next : t -> t

  include Hashable.S with type t := t
end = struct
  include Int

  let first = 0
  let next x = x + 1
end

module Cache = struct
  type t =
    { trace : Memtrace.Trace.Reader.t
    ; mutable next_location : T.t
    ; code_table : T.t list Code.Table.t
    ; data_table : Data.Location.t T.Table.t
    ; location_table : T.t Data.Location.Table.t
    }

  let create ~trace () =
    { trace
    ; next_location = T.first
    ; code_table = Code.Table.create ()
    ; data_table = T.Table.create ()
    ; location_table = Data.Location.Table.create ()
    }
  ;;

  let locs_from_code t loc_code : T.t list =
    Code.Table.find_or_add t.code_table loc_code ~default:(fun () ->
      let locs = Memtrace.Trace.Reader.lookup_location_code t.trace loc_code in
      List.map locs ~f:(fun loc_data ->
        let loc_data = convert_loc loc_data in
        Data.Location.Table.find_or_add t.location_table loc_data ~default:(fun () ->
          let loc = t.next_location in
          t.next_location <- T.next loc;
          T.Table.add_exn t.data_table ~key:loc ~data:loc_data;
          loc)))
  ;;

  let get_data t loc : Data.Location.t = T.Table.find_exn t.data_table loc
end

include T
