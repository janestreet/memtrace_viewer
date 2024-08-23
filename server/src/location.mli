open! Core
open Memtrace_viewer_common

type t : immediate

val allocator : t
val toplevel : t
val dummy : t (*_ required by [Substring_heavy_hitters] *)

include Comparable.S with type t := t
include Hashable.S with type t := t
include Sexpable.S with type t := t

module Code : sig
  type t = Memtrace.Trace.Location_code.t [@@deriving sexp_of]

  include Hashable.S_plain with type t := t
end

module Cache : sig
  type location := t
  type t

  val create : trace:Memtrace.Trace.Reader.t -> unit -> t
  val function_from_defname : t -> string -> location
  val call_sites_from_code : t -> Code.t -> Call_site.t list
  val get_function_of_call_site : t -> Call_site.t -> location
  val get_allocation_site_of_call_site : t -> Call_site.t -> location
  val get_defname : t -> location -> string
  val get_call_site_data : t -> Call_site.t -> Data.Call_site.t
  val get_loc_data : t -> location -> Data.Location.t
end
