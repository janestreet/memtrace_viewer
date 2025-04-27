(** Provides a way to pass an effect as a parameter of a component when the semantics of
    that effect depends on the value of the component. Instead of passing the effect
    directly, we can define some ['action] type and pass an _interpreter_ for ['action]
    into the component. We can then define the interpreter in terms of the value of the
    component, using [Bonsai.wrap] to tie the knot. *)

open! Core
open Bonsai_web_proc

module Interpreter : sig
  (** A function specifying the semantics of the action, in terms of the current value of
      the wrapped component and some extra context. Note that this is a pure function, in
      the Bonsai sense, so anything bound up in a [Value.t] must be passed in via the
      context. *)
  type ('value, 'action, 'context) t =
    value:'value -> context:'context -> 'action -> unit Effect.t
end

module Input : sig
  (** The input arguments (besides the action) for the interpreter. *)
  type ('value, 'context) t =
    { value : 'value
    (** The value of the wrapped component. Will also be returned to the outside world (by
        [component] below). *)
    ; context : 'context (** Extra context only needed internally by the interpreter. *)
    }
end

type ('value, 'action) t =
  { value : 'value
  ; run_action : 'action -> unit Effect.t
  }

val component
  :  interpret:('value, 'action, 'context) Interpreter.t
  -> input:
       (run_action:('action -> unit Effect.t) Value.t
        -> ('value, 'context) Input.t Computation.t)
  -> ('value, 'action) t Computation.t
