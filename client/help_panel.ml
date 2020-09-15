open! Core_kernel
open Bonsai_web

let component ~key_handler =
  let open Bonsai.Let_syntax in
  return
    (let%map key_handler = key_handler in
     let open Vdom in
     let keyboard_help_text =
       let text =
         Vdom_keyboard.Keyboard_event_handler.get_help_text
           ~include_disabled_keys:()
           key_handler
       in
       let view_spec =
         Vdom_keyboard.Help_text.View_spec.with_classes
           ~key_class:"keyboard-key"
           ~plain_text_class:"keyboard-text"
       in
       Vdom_keyboard.Help_text.view text view_spec
     in
     Node.section [ Attr.id "help-panel" ] [ keyboard_help_text ])
;;
