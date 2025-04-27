open! Core
open! Bonsai_web_proc

type t =
  { width : int
  ; height : int
  }
[@@deriving equal, sexp]

let get () =
  let open Js_of_ocaml in
  let width = Dom_html.window##.innerWidth in
  let height = Dom_html.window##.innerHeight in
  { width; height }
;;

let window_size = Bonsai.Var.create (get ())
let update_window_size () = Bonsai.Var.set window_size (get ())

let component =
  let open Bonsai.Let_syntax in
  let%sub () =
    Bonsai.Edge.after_display (Value.return (Effect.of_sync_fun update_window_size ()))
  in
  return (Bonsai.Var.value window_size |> Bonsai.Value.cutoff ~equal)
;;
