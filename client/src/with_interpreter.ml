open! Core
open Bonsai_web_proc

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
      ~apply_action:(fun apply_action_context result () action ->
        match result with
        | Inactive ->
          eprint_s
            [%message
              "An action sent to a [wrap] has been dropped because its input was not \
               present. This happens when the [wrap] is inactive when it receives a \
               message."
                [%here]];
          ()
        | Active (Input.{ value; context }, _) ->
          Bonsai.Apply_action_context.schedule_event
            apply_action_context
            (interpret ~value ~context action))
      ~f:(fun _ inject ->
        let%sub input = input ~run_action:inject in
        let%arr input
        and run_action = inject in
        input, run_action)
  in
  let%arr value and run_action in
  { value; run_action }
;;
