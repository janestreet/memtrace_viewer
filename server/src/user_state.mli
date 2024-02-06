open! Core
open! Async
open Memtrace_viewer_common

module Env : sig
  type t

  val of_trace : Memtrace.Trace.Reader.t -> t
end

type t

val data : t -> Data.t
val create : Env.t -> t
val reset : Env.t -> t -> unit
val update : Env.t -> t -> Action.t -> unit
