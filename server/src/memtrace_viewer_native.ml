open! Core
open! Async
open Memtrace
module Time = Time_float_unix
module Socket = Async_unix.Unix.Socket

let initialize_connection env _ _ _ _ = User_state.create env

let log_request ?(log = Lazy.force Log.Global.log) inet path =
  [%log.t.debug
    log "Serving http request" (inet : Socket.Address.Inet.t) (Time.now () : Time.t) path]
;;

let respond_string ~content_type ?flush ?headers ?status s =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Cohttp_async.Server.respond_string ?flush ~headers ?status s
;;

let not_found_html =
  {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <title>404 Not Found</title>
  </head>
  <body>
    <h1>404 Not Found</h1>
  </body>
</html>
|}
;;

let html =
  {|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <script defer src="main.js"></script>
    <link rel="stylesheet" type="text/css" href="style.css">
  </head>

  <body>
    <div id="app"></div>
  </body>
</html>
|}
;;

let handler ~body:_ inet req =
  let path = Uri.path (Cohttp.Request.uri req) in
  log_request inet path;
  match path with
  | "" | "/" | "/index.html" -> respond_string ~content_type:"text/html" html
  | "/main.js" ->
    respond_string
      ~content_type:"application/javascript"
      Embedded_files.main_dot_bc_dot_js
  | "/style.css" -> respond_string ~content_type:"text/css" Embedded_files.style_dot_css
  | _ -> respond_string ~content_type:"text/html" ~status:`Not_found not_found_html
;;

let rec search_for_port_exn ~port ~end_port =
  assert (port <= end_port);
  let socket = Socket.create Socket.Type.tcp in
  Socket.setopt socket Socket.Opt.reuseaddr true;
  match Socket.bind_inet_keep_opts socket (Socket.Address.Inet.create_bind_any ~port) with
  | exception Unix.Unix_error (EADDRINUSE, _, _) when port < end_port ->
    search_for_port_exn ~port:(port + 1) ~end_port
  | sock ->
    let%bind () = Fd.close (Socket.fd sock) in
    return port
;;

let main ~filename ~port =
  let port, end_port =
    match port with
    | Some port -> port, port
    | None -> 8080, 9000
  in
  let%bind port = search_for_port_exn ~port ~end_port in
  Core.Printf.printf "Processing %s...\n%!" filename;
  let trace = Trace.Reader.open_ ~filename in
  let env = User_state.Env.of_trace trace in
  let hostname = Unix.gethostname () in
  let heartbeat_config =
    Rpc.Connection.Heartbeat_config.create
      ~timeout:(Time_ns.Span.of_int_day 7)
      ~send_every:(Time_ns.Span.of_int_day 1)
      ()
  in
  printf "Serving http://%s:%d/\n%!" hostname port;
  let%bind server =
    let http_handler () = handler in
    Rpc_websocket.Rpc.serve
      ~heartbeat_config
      ~on_handler_error:`Ignore
      ~mode:`TCP
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~http_handler
      ~implementations:(Rpc_implementations.implementations env)
      ~initial_connection_state:(initialize_connection env)
      ()
  in
  let%map () = Cohttp_async.Server.close_finished server in
  Trace.Reader.close trace
;;

let command =
  Command.async
    ~summary:"Start server for memtrace viewer"
    (let%map_open.Command filename = anon ("filename" %: string)
     and port = flag "port" (optional int) ~doc:"port on which to serve viewer" in
     fun () -> main ~filename ~port)
    ~behave_nicely_in_pipeline:false
;;

module User_state = User_state

module For_testing = struct
  module Filtered_trace = Filtered_trace
  module Location = Location
  module Raw_trace = Raw_trace
  module Substring_heavy_hitters = Substring_heavy_hitters
end
