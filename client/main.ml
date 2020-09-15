open! Core_kernel
open! Async_kernel
open Bonsai_web
open Async_js
open Memtrace_viewer_common

let initialize ~conn ~data =
  let%map data' = Rpc.Rpc.dispatch_exn Protocol.Init.t conn () in
  Bonsai.Var.update data ~f:(fun _ -> data')
;;

let handle_outgoing ~conn ~data ~server_state action =
  match%map Rpc.Rpc.dispatch Protocol.Update.t conn action with
  | Ok data' -> Bonsai.Var.set data data'
  | Error error ->
    Js_of_ocaml.Firebug.console##error
      (error |> Error.to_string_hum |> Js_of_ocaml.Js.string);
    Bonsai.Var.set server_state { Server_state.status = Down }
;;

let run () =
  (* need to call this before [Rpc.Connection.client_exn] *)
  Async_js.init ();
  let data = Bonsai.Var.create Data.empty in
  let server_state = Bonsai.Var.create Server_state.initial in
  let%bind conn = Rpc.Connection.client_exn () in
  let inject_outgoing =
    let handle_outgoing_staged =
      Effect.of_deferred_fun (handle_outgoing ~conn ~data ~server_state)
    in
    fun action ->
      Effect.inject_ignoring_response (Staged.unstage handle_outgoing_staged action)
  in
  let _app_handle =
    Start.start
      Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      (App.component
         ~data:(Bonsai.Var.value data)
         ~server_state:(Bonsai.Var.value server_state)
         ~inject_outgoing:(Bonsai.Value.return inject_outgoing))
  in
  let%map () = initialize ~conn ~data in
  ()
;;

let () =
  Incr_dom.Incr.State.set_max_height_allowed Incr_dom.Incr.State.t 1024;
  don't_wait_for (run ())
;;
