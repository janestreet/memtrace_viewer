open! Core_kernel
open Bonsai_web

type 'a t =
  { current : 'a
  ; set : 'a -> Vdom.Event.t
  }

module Action (Model : Bonsai_web.Bonsai.Model) = struct
  type t = Set of Model.t [@@deriving sexp_of]
end

let component (type a) (module Model : Bonsai_web.Bonsai.Model with type t = a) ~initial
  : a t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let module Action = Action (Model) in
  let apply_action ~inject:_ ~schedule_event:_ _ action =
    match action with
    | Action.Set new_state -> new_state
  in
  let%sub current_and_inject =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Action)
      ~default_model:initial
      ~apply_action
  in
  Bonsai.Let_syntax.return
    (let%map current, inject = current_and_inject in
     let set new_state = inject (Action.Set new_state) in
     { current; set })
;;
