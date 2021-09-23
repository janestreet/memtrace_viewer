open! Core
open! Async_rpc_kernel

module Init : sig
  val t : (unit, Data.Serialized.t) Rpc.Rpc.t
end

module Update : sig
  val t : (Action.t, Data.Serialized.t) Rpc.Rpc.t
end
