open! Core
open! Bonsai_web_proc

module Button_position : sig
  type t =
    | Left
    | Right
end

val component
  :  ?attr:Vdom.Attr.t Value.t
  -> Vdom.Node.t list Value.t
  -> button_position:Button_position.t
  -> Vdom.Node.t Computation.t
