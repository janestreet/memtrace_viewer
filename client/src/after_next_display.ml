open! Core
open Bonsai_web

module Action = struct
  type t =
    | Activate
    | Run_if_active
  [@@deriving sexp_of]
end

module Model = struct
  type t = { activated : bool } [@@deriving sexp, equal, compare]
end

let component effect =
  let open Bonsai.Let_syntax in
  let%sub _, run_action =
    Bonsai.state_machine1
      (module Model)
      (module Action)
      ~default_model:{ activated = false }
      ~apply_action:(fun ~inject:_ ~schedule_event input model action ->
        match action with
        | Activate -> { activated = true }
        | Run_if_active ->
          if model.activated
          then (
            schedule_event input;
            { activated = false })
          else model)
      effect
  in
  let run_if_active =
    let%map run_action = run_action in
    run_action Run_if_active
  in
  let%sub () = Bonsai.Edge.after_display run_if_active in
  let%arr run_action = run_action in
  run_action Activate
;;
