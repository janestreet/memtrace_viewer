open! Core_kernel
open! Async_rpc_kernel

module Init : sig
  val t : (unit, Data.t) Rpc.Rpc.t
end

module Update : sig
  val t : (Action.t, Data.t) Rpc.Rpc.t
end
