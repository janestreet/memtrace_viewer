open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  }

let zoom_in_behavior ~(data : Data.t) ~focus =
  match focus with
  | None -> `Disabled
  | Some (focus, _) ->
    let focus_node = Data.Trie.find data.trie focus in
    (match focus_node with
     | Some focus_node
       when not (Data.Location.Map.is_empty (Data.Trie.Node.children focus_node)) ->
       `Set_zoom focus
     | _ -> `Disabled)
;;

let zoom_out_behavior zoom =
  match zoom with
  | None -> `Disabled
  | Some zoom ->
    (* If the new backtrace would be empty, set the zoom to None instead *)
    let next_zoom =
      match List.rev zoom with
      | _ :: (_ :: _ as next_zoom_rev) -> Some (List.rev next_zoom_rev)
      | _ -> None
    in
    `Set_zoom next_zoom
;;

let button_bar ~data ~focus ~zoom ~set_zoom =
  let open Vdom in
  let zoom_button =
    let on_click_or_disabled =
      match zoom_in_behavior ~data ~focus with
      | `Set_zoom focus -> Attr.on_click (fun _ -> set_zoom (Some focus))
      | `Disabled -> Attr.disabled
    in
    Node.button
      [ on_click_or_disabled; Attr.title "Zoom into the selected function" ]
      [ Node.text "Zoom In" ]
  in
  let zoom_out_button =
    let on_click_or_disabled =
      match zoom_out_behavior zoom with
      | `Set_zoom next_zoom -> Attr.on_click (fun _ -> set_zoom next_zoom)
      | `Disabled -> Attr.disabled
    in
    Node.button
      [ on_click_or_disabled; Attr.title "Zoom out by one level" ]
      [ Node.text "Zoom Out" ]
  in
  let reset_zoom_button =
    let on_click_or_disabled =
      match zoom with
      | Some _ -> Attr.on_click (fun _ -> set_zoom None)
      | None -> Attr.disabled
    in
    Node.button
      [ on_click_or_disabled; Attr.title "Zoom all the way out" ]
      [ Node.text "Reset Zoom" ]
  in
  Node.div
    [ Attr.class_ "button-bar" ]
    [ zoom_button; zoom_out_button; reset_zoom_button ]
;;

let zoom_view ~(data : Data.t) ~zoom =
  let zoom_node =
    let%bind.Option backtrace = zoom in
    Data.Trie.find data.trie backtrace
  in
  match zoom, zoom_node with
  | Some backtrace, Some node ->
    let direction = data.direction in
    let total_line =
      Vdom.Node.p
        [ Vdom.Attr.class_ "total-allocations" ]
        [ Vdom.Node.textf
            "Total allocations within zoom: %s"
            (Data.Entry.allocations (Data.Trie.Node.entry node)
             |> Byte_units.Short.to_string)
        ]
    in
    Vdom.Node.div [] [ total_line; Backtrace_view.render ~direction backtrace ]
  | _, _ ->
    (* Be consistent with other components that ignore the zoom when it's not in the
       trie (presumably because filter has changed since the zoom was chosen) *)
    Vdom.Node.none
;;

let view ~data ~focus ~zoom ~set_zoom =
  let open Vdom in
  Node.section
    [ Attr.id "zoom-panel" ]
    [ Node.h2 [] [ Node.text "Zoom" ]
    ; zoom_view ~data ~zoom
    ; button_bar ~data ~focus ~zoom ~set_zoom
    ]
;;

let zoom_action ~data ~focus ~set_zoom =
  let key = Vdom_keyboard.Keystroke.create' Enter in
  match zoom_in_behavior ~data ~focus with
  | `Disabled -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
  | `Set_zoom focus ->
    let keys = [ key ] in
    let description = "Zoom" in
    let group = None in
    let handler _ = set_zoom (Some focus) in
    Command { keys; description; group; handler }
;;

let zoom_out_action ~zoom ~set_zoom =
  let key = Vdom_keyboard.Keystroke.create' ~shift:() Enter in
  match zoom_out_behavior zoom with
  | `Disabled -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
  | `Set_zoom next_zoom ->
    let keys = [ key ] in
    let description = "Zoom Out" in
    let group = None in
    let handler _ = set_zoom next_zoom in
    Command { keys; description; group; handler }
;;

let reset_zoom_action ~zoom ~set_zoom =
  let key = Vdom_keyboard.Keystroke.create' KeyR in
  match zoom with
  | None -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
  | Some _ ->
    let keys = [ key ] in
    let description = "Reset Zoom" in
    let group = None in
    let handler _ = set_zoom None in
    Command { keys; description; group; handler }
;;

let key_handler ~data ~focus ~zoom ~set_zoom =
  Vdom_keyboard.Keyboard_event_handler.of_action_list_exn
    [ zoom_action ~data ~focus ~set_zoom
    ; zoom_out_action ~zoom ~set_zoom
    ; reset_zoom_action ~zoom ~set_zoom
    ]
;;

let component ~data ~focus ~zoom ~set_zoom =
  let open Bonsai.Let_syntax in
  return
    (let%map data = data
     and focus = focus
     and zoom = zoom
     and set_zoom = set_zoom in
     let view = view ~data ~focus ~zoom ~set_zoom in
     let key_handler = key_handler ~data ~focus ~zoom ~set_zoom in
     { view; key_handler })
;;

let view { view; _ } = view
let key_handler { key_handler; _ } = key_handler
