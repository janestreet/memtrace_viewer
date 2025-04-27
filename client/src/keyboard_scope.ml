open! Core
open Bonsai_web_proc

type t =
  { view : Vdom.Node.t
  ; key_help : Vdom_keyboard.Help_text.t
  }

let wrap ~view ~key_handler =
  let open Vdom in
  let on_keydown event =
    match Vdom_keyboard.Keyboard_event_handler.handle_event key_handler event with
    | Some action -> Effect.Many [ action; Effect.Stop_propagation ]
    | None -> Effect.Ignore
  in
  let view =
    Node.div
      ~attrs:
        [ Attr.many_without_merge
            [ (* Make focusable *) Attr.tabindex (-1); Attr.on_keydown on_keydown ]
        ]
      [ view ]
  in
  let key_help = Vdom_keyboard.Keyboard_event_handler.get_help_text key_handler in
  { view; key_help }
;;
