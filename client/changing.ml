open! Core_kernel
open Bonsai_web

module type Value = sig
  type t [@@deriving sexp, equal]
end

type 'a t =
  { value : 'a
  ; set_value : 'a -> Vdom.Event.t
  ; changing : bool
  ; set_changing : bool -> Vdom.Event.t
  }
[@@deriving fields]

module Component (Value : Value) = struct
  let name = "Changing value"

  module Input = Unit

  module Result = struct
    type nonrec t = Value.t t
  end

  module Action = struct
    type t =
      | Set of Value.t
      | Set_changing of bool
    [@@deriving sexp_of]
  end

  module Model = struct
    type t =
      { value : Value.t
      ; changing : bool
      }
    [@@deriving sexp, equal]

    let default ~initial_value = { value = initial_value; changing = false }
  end

  let apply_action ~inject:_ ~schedule_event:_ _ (model : Model.t) (action : Action.t)
    : Model.t
    =
    match action with
    | Set value -> { value; changing = true }
    | Set_changing changing -> { model with changing }
  ;;

  let compute ~inject () Model.{ value; changing } : Result.t =
    let set_value value = inject (Action.Set value) in
    let set_changing changing = inject (Action.Set_changing changing) in
    { value; set_value; changing; set_changing }
  ;;
end

let component (type a) (module Value : Value with type t = a) ~initial_value =
  let module C = Component (Value) in
  let default_model = C.Model.default ~initial_value in
  Bonsai.of_module0 (module C) ~default_model
;;
