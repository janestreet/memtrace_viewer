open! Core_kernel
open! Async
open Memtrace_viewer_common

let init global_state =
  let f user_state () =
    User_state.reset global_state user_state;
    return (User_state.data user_state)
  in
  Rpc.Rpc.implement Protocol.Init.t f
;;

let update global_state =
  let f user_state action =
    User_state.update global_state user_state action;
    return (User_state.data user_state)
  in
  Rpc.Rpc.implement Protocol.Update.t f
;;

let implementations global_state =
  Rpc.Implementations.create_exn
    ~implementations:[ init global_state; update global_state ]
    ~on_unknown_rpc:`Continue
;;
