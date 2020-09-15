open! Core_kernel
open! Async
open Memtrace_viewer_common

module Initial : sig
  type t

  val of_trace : Memtrace.Trace.Reader.t -> t
end

type t

val data : t -> Data.t
val create : initial_state:Initial.t -> filter:Filter.t -> t
val reset : Initial.t -> t -> unit
val update : Initial.t -> t -> Action.t -> unit
