open! Core
open! Async_kernel
open Bonsai_web_proc
open Async_js
open Memtrace_viewer_common

let initialize ~conn ~data =
  let%map serialized = Rpc.Rpc.dispatch_exn Protocol.Init.t conn () in
  let unserialized = Data.Serialized.unserialize serialized in
  Bonsai.Var.update data ~f:(fun _ -> unserialized)
;;

let handle_outgoing ~conn ~data ~server_state action =
  Bonsai.Var.set server_state { Server_state.status = Busy };
  match%map Rpc.Rpc.dispatch Protocol.Update.t conn action with
  | Ok serialized ->
    let unserialized = Data.Serialized.unserialize serialized in
    Bonsai.Var.set data unserialized;
    Bonsai.Var.set server_state { Server_state.status = Idle }
  | Error error ->
    Js_of_ocaml.Console.console##error
      (error |> Error.to_string_hum |> Js_of_ocaml.Js.string);
    Bonsai.Var.set server_state { Server_state.status = Down }
;;

let run () =
  (* need to call this before [Rpc.Connection.client_exn] *)
  Async_js.init ();
  let data = Bonsai.Var.create Data.empty in
  let server_state = Bonsai.Var.create Server_state.initial in
  let heartbeat_config =
    Rpc.Connection.Heartbeat_config.create
      ~timeout:(Time_ns.Span.of_int_day 7)
      ~send_every:(Time_ns.Span.of_int_day 1)
      ()
  in
  let%bind conn = Rpc.Connection.client_exn ~heartbeat_config () in
  let inject_outgoing =
    let handle_outgoing_staged =
      Effect.of_deferred_fun (handle_outgoing ~conn ~data ~server_state)
    in
    fun action -> handle_outgoing_staged action
  in
  Bonsai_web.Start.start
    (App.component
       ~data:(Bonsai.Var.value data)
       ~server_state:(Bonsai.Var.value server_state)
       ~inject_outgoing:(Bonsai.Value.return inject_outgoing));
  let%map () = initialize ~conn ~data in
  ()
;;

let main () =
  Incr_dom.Incr.State.set_max_height_allowed Incr_dom.Incr.State.t 1024;
  don't_wait_for (run ())
;;

module App = App
module Server_state = Server_state

module For_testing = struct
  module Graph_view = Graph_view
end
