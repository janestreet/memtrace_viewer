open! Core
open Bonsai_web

module Interpreter = struct
  type ('value, 'action, 'context) t =
    value:'value -> context:'context -> 'action -> unit Effect.t
end

module Input = struct
  type ('value, 'context) t =
    { value : 'value
    ; context : 'context
    }
end

type ('value, 'action) t =
  { value : 'value
  ; run_action : 'action -> unit Effect.t
  }

let component ~interpret ~input =
  let open Bonsai.Let_syntax in
  let%sub Input.{ value; context = _ }, run_action =
    Bonsai.wrap
      ()
      ~equal:[%equal: Unit.t]
      ~default_model:()
      ~apply_action:
        (fun
          ~inject:_ ~schedule_event (Input.{ value; context }, _) () action ->
          schedule_event (interpret ~value ~context action))
      ~f:(fun _ inject ->
        let%sub input = input ~run_action:inject in
        let%arr input = input
        and run_action = inject in
        input, run_action)
  in
  let%arr value = value
  and run_action = run_action in
  { value; run_action }
;;
