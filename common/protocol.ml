open! Core_kernel
open! Async_rpc_kernel

module Init = struct
  let t =
    Rpc.Rpc.create
      ~name:"init"
      ~version:0
      ~bin_query:[%bin_type_class: Unit.t]
      ~bin_response:[%bin_type_class: Data.t]
  ;;
end

module Update = struct
  let t =
    Rpc.Rpc.create
      ~name:"update"
      ~version:0
      ~bin_query:[%bin_type_class: Action.t]
      ~bin_response:[%bin_type_class: Data.t]
  ;;
end
