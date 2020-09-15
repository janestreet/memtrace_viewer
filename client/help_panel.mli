open! Core_kernel
open Bonsai_web

val component
  :  key_handler:Vdom_keyboard.Keyboard_event_handler.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
