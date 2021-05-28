open! Core_kernel
open Bonsai_web

type t =
  { view : Vdom.Node.t
  ; key_help : Vdom_keyboard.Help_text.t
  }

val wrap : view:Vdom.Node.t -> key_handler:Vdom_keyboard.Keyboard_event_handler.t -> t
