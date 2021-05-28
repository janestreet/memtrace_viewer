open! Core_kernel
open Memtrace_viewer_common

type t

val allocation_site : t
val toplevel : t
val dummy : t (* required by [Substring_heavy_hitters] *)

include Comparable.S with type t := t
include Hashable.S with type t := t
include Sexpable.S with type t := t

module Code : sig
  type t = Memtrace.Trace.Location_code.t

  include Hashable.S_plain with type t := t
end

module Cache : sig
  type location := t
  type t

  val create : trace:Memtrace.Trace.Reader.t -> unit -> t
  val loc_from_data : t -> Data.Location.t -> location
  val locs_from_code : t -> Code.t -> location list
  val get_data : t -> location -> Data.Location.t
end
