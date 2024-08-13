open! Core
open! Async
open Memtrace_viewer_common

let init global_state =
  let f user_state () =
    User_state.reset global_state user_state;
    let data = User_state.data user_state in
    let serialized = Data.Serialized.serialize data in
    return serialized
  in
  Rpc.Rpc.implement Protocol.Init.t f
;;

let update global_state =
  let f user_state action =
    User_state.update global_state user_state action;
    let data = User_state.data user_state in
    let serialized = Data.Serialized.serialize data in
    return serialized
  in
  Rpc.Rpc.implement Protocol.Update.t f
;;

let implementations global_state =
  Rpc.Implementations.create_exn
    ~implementations:[ init global_state; update global_state ]
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;
