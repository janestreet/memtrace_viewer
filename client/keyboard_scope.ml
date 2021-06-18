open! Core
open Bonsai_web

type t =
  { view : Vdom.Node.t
  ; key_help : Vdom_keyboard.Help_text.t
  }

let wrap ~view ~key_handler =
  let open Vdom in
  let on_keydown event =
    match Vdom_keyboard.Keyboard_event_handler.handle_event key_handler event with
    | Some action -> Event.Many [ action; Event.Stop_propagation ]
    | None -> Event.Ignore
  in
  let view =
    Node.div
      ~attr:
        (Attr.many_without_merge
           [ (* Make focusable *) Attr.tabindex (-1); Attr.on_keydown on_keydown ])
      [ view ]
  in
  let key_help = Vdom_keyboard.Keyboard_event_handler.get_help_text key_handler in
  { view; key_help }
;;
