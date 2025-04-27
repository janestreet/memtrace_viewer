open! Core
open! Bonsai_web_proc

module Button_position = struct
  type t =
    | Left
    | Right
end

module State = struct
  type t =
    | Collapsed
    | Expanded
  [@@deriving sexp, compare, equal]

  let flip = function
    | Collapsed -> Expanded
    | Expanded -> Collapsed
  ;;
end

let left_arrow = Vdom.Node.text "❰"
let right_arrow = Vdom.Node.text "❱"

let view ~attr ~nodes ~(button_position : Button_position.t) ~state ~set_state =
  let open Vdom in
  let on_click _ = set_state (State.flip state) in
  let arrow =
    match state, button_position with
    | Expanded, Right | Collapsed, Left -> left_arrow
    | Collapsed, Right | Expanded, Left -> right_arrow
  in
  let button =
    Node.button
      ~attrs:[ Attr.classes [ "sidebar-button"; "flat-button" ]; Attr.on_click on_click ]
      [ arrow ]
  in
  let button_strip = Node.div ~attrs:[ Attr.class_ "sidebar-button-strip" ] [ button ] in
  let state_class =
    match state with
    | Expanded -> "sidebar-expanded"
    | Collapsed -> "sidebar-collapsed"
  in
  let content = Node.div ~attrs:[ Attr.class_ "sidebar-content" ] nodes in
  let parts =
    match button_position with
    | Left -> [ button_strip; content ]
    | Right -> [ content; button_strip ]
  in
  Node.div parts ~attrs:[ attr; Attr.classes [ "sidebar"; state_class ] ]
;;

let component ?(attr = Value.return Vdom.Attr.empty) nodes ~button_position =
  let open Bonsai.Let_syntax in
  let%sub state, set_state = Bonsai.state Expanded ~equal:[%equal: State.t] in
  return
    (let%map attr and nodes and state and set_state in
     view ~attr ~nodes ~button_position ~state ~set_state)
;;
