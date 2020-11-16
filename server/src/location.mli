open! Core_kernel
open Memtrace_viewer_common

type t

include Hashable.S with type t := t

module Code : sig
  type t = Memtrace.Trace.Location_code.t

  include Hashable.S_plain with type t := t
end

module Cache : sig
  type location := t
  type t

  val create : trace:Memtrace.Trace.Reader.t -> unit -> t
  val locs_from_code : t -> Code.t -> location list
  val get_data : t -> location -> Data.Location.t
end
