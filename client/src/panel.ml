open! Core
open Bonsai_web_proc

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

module Collapsible = struct
  type t =
    | No
    | Yes of { initial_state : State.t }
end

let title_bar ~title ~collapsible ~state ~set_state : Vdom.Node.t option Computation.t =
  let open Bonsai.Let_syntax in
  return
    (let%map title and state and set_state in
     Option.map title ~f:(fun title ->
       let open Vdom in
       let on_click_attr =
         match (collapsible : Collapsible.t) with
         | No -> Attr.empty
         | Yes _ -> Attr.on_click (fun _ -> set_state (State.flip state))
       in
       let collapse_indicator_text =
         match collapsible, state with
         | No, _ -> None
         | Yes _, Collapsed -> Some "▸" (* u+25b8 BLACK RIGHT-POINTING SMALL TRIANGLE *)
         | Yes _, Expanded -> Some "▾"
         (* u+25be BLACK DOWN-POINTING SMALL TRIANGLE *)
       in
       let collapse_indicator =
         match collapse_indicator_text with
         | Some text ->
           (* Attach the same event handler to handle keyboard activation *)
           Node.button
             ~attrs:[ on_click_attr; Attr.class_ "panel-collapse-indicator" ]
             [ Node.text text ]
         | None -> Node.none_deprecated [@alert "-deprecated"]
       in
       Node.h2
         ~attrs:[ on_click_attr; Attr.class_ "panel-title" ]
         [ collapse_indicator; Node.text title ]))
;;

let panel ?title body ~id ~(collapsible : Collapsible.t) =
  let open Bonsai.Let_syntax in
  let%sub state, set_state =
    let default_model : State.t =
      match collapsible with
      | No -> Expanded
      | Yes { initial_state; _ } -> initial_state
    in
    Bonsai.state default_model ~equal:[%equal: State.t]
  in
  let title =
    match title with
    | None -> Value.return None
    | Some title ->
      let%map title in
      Some title
  in
  let%sub title_bar = title_bar ~title ~collapsible ~state ~set_state in
  return
    (let%map title_bar and body and state in
     let open Vdom in
     let collapsible_class =
       match collapsible with
       | Yes _ -> "panel-collapsible"
       | No -> "panel-not-collapsible"
     in
     let state_class =
       match state with
       | Collapsed -> "panel-collapsed"
       | Expanded -> "panel-expanded"
     in
     Node.section
       ~attrs:[ Attr.classes [ "panel"; state_class; collapsible_class ]; Attr.id id ]
       [ Node.div
           ~attrs:[ Attr.class_ "panel-content" ]
           (List.concat
              [ title_bar |> Option.to_list
              ; [ Node.div ~attrs:[ Attr.class_ "panel-body" ] [ body ] ]
              ])
       ])
;;
