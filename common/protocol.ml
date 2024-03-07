open! Core
open! Async_rpc_kernel

module Init = struct
  let t =
    Rpc.Rpc.create
      ~name:"init"
      ~version:0
      ~bin_query:[%bin_type_class: Unit.t]
      ~bin_response:[%bin_type_class: Data.Serialized.t]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Update = struct
  let t =
    Rpc.Rpc.create
      ~name:"update"
      ~version:0
      ~bin_query:[%bin_type_class: Action.t]
      ~bin_response:[%bin_type_class: Data.Serialized.t]
      ~include_in_error_count:Only_on_exn
  ;;
end
