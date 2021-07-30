open! Core
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  }

module Orientations = Poi_panel.Orientations
module Selection = Main_panel.Selection

module Extend_or_retract = struct
  type t =
    | Extend
    | Retract
  [@@deriving equal]
end

type focus_behavior =
  | Action of (unit -> unit Ui_effect.t)
  | Disabled

let selection_focus_behavior_flame_graph
      ~sort
      ~(orient : Data.Orientation.t option)
      ~(selection : Selection.Flame_graph.t)
  =
  match selection with
  | Flame { extend_focus_to; _ } ->
    if
      Option.for_all ~f:(Extend_or_retract.equal Extend) sort
      && Option.for_all ~f:(Data.Orientation.equal Callees) orient
    then Action extend_focus_to
    else Disabled
  | Icicle { extend_focus_to; _ } ->
    if
      Option.for_all ~f:(Extend_or_retract.equal Extend) sort
      && Option.for_all ~f:(Data.Orientation.equal Callers) orient
    then Action extend_focus_to
    else Disabled
  | Focus { retract_callees_from_focus; retract_callers_from_focus; _ } ->
    if Option.for_all ~f:(Extend_or_retract.equal Retract) sort
    then (
      let act_opt =
        match orient with
        | Some Callees | None -> retract_callees_from_focus
        | Some Callers -> retract_callers_from_focus
      in
      match act_opt with
      | None -> Disabled
      | Some act -> Action act)
    else Disabled
;;

let selection_focus_behavior_table ~(selection : Selection.Table.t) =
  match selection with
  | { extend_focus_to = Some act; _ } -> Action act
  | { extend_focus_to = None; _ } -> Disabled
;;

let selection_focus_behavior ~sort ~orient ~(selection : Selection.t) =
  match selection with
  | Flame_graph { selection = Some selection } ->
    selection_focus_behavior_flame_graph ~sort ~orient ~selection
  | Table { selection = Some selection; orient = table_orient; _ } ->
    if
      Option.for_all ~f:(Extend_or_retract.equal Extend) sort
      && Option.for_all ~f:(Data.Orientation.equal table_orient) orient
    then selection_focus_behavior_table ~selection
    else Disabled
  | Flame_graph { selection = None } | Table { selection = None; _ } -> Disabled
;;

let extend_focus_behavior ~orient ~selection =
  selection_focus_behavior ~sort:(Some Extend) ~orient:(Some orient) ~selection
;;

let retract_focus_behavior ~orient ~selection =
  selection_focus_behavior ~sort:(Some Retract) ~orient:(Some orient) ~selection
;;

let retract_focus_by_one_behavior ~(selection : Selection.t) =
  match selection with
  | Table { retract_from_focus = Some act; _ } -> Action act
  | Flame_graph _ | Table { retract_from_focus = None; _ } -> Disabled
;;

let reset_focus_behavior ~poi ~focus ~set_focus =
  if Data.Fragment.same focus poi then Disabled else Action (fun () -> set_focus poi)
;;

let refocus_behavior ~trie ~selection ~focus ~set_focus =
  match Selection.location selection with
  | None -> Disabled
  | Some first ->
    let fragment =
      match Data.Fragment_trie.find_singleton trie first with
      | Some fragment -> fragment
      | None -> assert false
    in
    if Data.Fragment.same fragment focus
    then Disabled
    else Action (fun () -> set_focus fragment)
;;

let extend_callees_symbol = (* BLACK UP-POINTING TRIANGLE *) "\u{25b2}"
let extend_callers_symbol = (* BLACK DOWN-POINTING TRIANGLE *) "\u{25bc}"
let retract_callees_symbol = extend_callers_symbol
let retract_callers_symbol = extend_callees_symbol

let button_bar ~trie ~(orientations : Orientations.t) ~selection ~poi ~focus ~set_focus =
  let open Vdom in
  let focus_button ~behavior ~text =
    let attr =
      match behavior with
      | Action act -> Attr.on_click (fun _ -> act ())
      | Disabled -> Attr.disabled
    in
    Node.button ~attr [ Node.text text ]
  in
  let double_button ~(f : orient:Data.Orientation.t -> Vdom.Node.t) =
    let top_button = f ~orient:Callees in
    let bottom_button = f ~orient:Callers in
    Node.div ~attr:(Attr.class_ "double-button") [ top_button; bottom_button ]
  in
  (* Having two buttons for extending the focus isn't essential; just seems a
     little clearer to have the controls for the flame part and the controls
     for the icicle part. Also, the triangles on the retract buttons are (more)
     unambiguous this way. *)
  let extend_focus_buttons =
    match orientations with
    | Only orient ->
      let behavior = extend_focus_behavior ~selection ~orient in
      focus_button ~behavior ~text:"Zoom in to selected"
    | Both ->
      (match selection with
       | Table { orient; _ } ->
         let behavior = extend_focus_behavior ~orient ~selection in
         focus_button ~behavior ~text:"Zoom in to selected"
       | Flame_graph _ ->
         double_button ~f:(fun ~orient ->
           let behavior = extend_focus_behavior ~orient ~selection in
           let dir_symbol =
             match orient with
             | Callees -> extend_callees_symbol
             | Callers -> extend_callers_symbol
           in
           let text = sprintf "Extend focus %s to selected" dir_symbol in
           focus_button ~behavior ~text))
  in
  let retract_focus_buttons =
    match selection with
    | Table _ ->
      let behavior = retract_focus_by_one_behavior ~selection in
      focus_button ~behavior ~text:"Zoom out"
    | Flame_graph _ ->
      (match orientations with
       | Only orient ->
         let behavior = retract_focus_behavior ~orient ~selection in
         focus_button ~behavior ~text:"Zoom out to selected"
       | Both ->
         double_button ~f:(fun ~orient ->
           let behavior = retract_focus_behavior ~orient ~selection in
           let text =
             let dir_symbol =
               match orient with
               | Data.Orientation.Callees -> retract_callees_symbol
               | Callers -> retract_callers_symbol
             in
             sprintf "Retract focus %s to selected" dir_symbol
           in
           focus_button ~behavior ~text))
  in
  let reset_focus_button =
    let behavior = reset_focus_behavior ~poi ~focus ~set_focus in
    let text =
      match orientations with
      | Only _ -> "Reset zoom to point of interest"
      | Both -> "Reset focus to point of interest"
    in
    focus_button ~behavior ~text
  in
  let refocus_button =
    match selection with
    | Table _ -> Node.None
    | Flame_graph _ ->
      (match orientations with
       | Only _ -> Node.None
       | Both ->
         let behavior = refocus_behavior ~trie ~selection ~focus ~set_focus in
         let text = "Focus on selected node only" in
         focus_button ~behavior ~text)
  in
  Node.div
    ~attr:(Attr.class_ "button-bar")
    [ extend_focus_buttons; retract_focus_buttons; reset_focus_button; refocus_button ]
;;

(* sequestering these here to isolate the confusing bits *)
let focus_label ~orientations =
  match orientations with
  | Orientations.Only _ ->
    (* There is only one direction so buttons like "zoom in"
       make the most sense *)
    "Zoomed"
  | Both ->
    (* This is a double flame graph, so the zoom metaphor breaks down *)
    "Focused"
;;

let selection_label = "Selected"
let en_dash = "\u{2013}"

let selection_fragment_flame_graph ~focus : Selection.Flame_graph.t -> _ = function
  | Flame { fragment; _ } | Icicle { fragment; _ } -> fragment
  | Focus _ -> focus
;;

let selection_fragment ~focus : Selection.t -> _ = function
  | Flame_graph { selection = Some selection } ->
    Some (selection_fragment_flame_graph ~focus selection)
  | Table { selection = Some { fragment; _ }; _ } -> Some fragment
  | Flame_graph { selection = None } | Table { selection = None; _ } -> None
;;

let info_view ~orientations ~selection ~focus ~total_allocations =
  let open Vdom in
  let line ?(attrs = Attr.empty) label nodes =
    Node.p
      (List.concat
         [ [ Node.span
               ~attr:(Attr.many_without_merge Attr.[ class_ "info-label" @ attrs ])
               [ Node.textf "%s: " label ]
           ]
         ; nodes
         ])
  in
  let to_percent size = 100. *. Byte_units.(size // total_allocations) in
  let allocations_line label ~fragment =
    let value_part =
      match fragment with
      | Some fragment ->
        let allocs = Data.Entry.allocations (Data.Fragment.entry fragment) in
        Node.textf
          "%s (%.1f%%)"
          (allocs |> Byte_units.Short.to_string)
          (allocs |> to_percent)
      | None -> Vdom.Node.text en_dash
    in
    line label [ value_part ] ~attrs:(Attr.class_ "indented")
  in
  let focus_line = line (focus_label ~orientations) [] in
  let focus_value = Backtrace_view.render (Data.Fragment.backtrace focus) in
  let focus_allocations_line =
    let trivial = Data.Fragment.is_trivial focus in
    let fragment = Option.some_if (not trivial) focus in
    allocations_line "Total allocations" ~fragment
  in
  let selection_line = line selection_label [] in
  let selection_value =
    let contents =
      match Selection.location selection with
      | Some selection -> Location.format_dom selection
      | None -> Node.span ~attr:(Attr.class_ "loc-special") [ Node.text "(none)" ]
    in
    Node.p ~attr:(Attr.class_ "indented") [ contents ]
  in
  let selection_allocations_line =
    let fragment = selection_fragment ~focus selection in
    allocations_line "Allocations" ~fragment
  in
  Node.div
    [ focus_line
    ; focus_value
    ; focus_allocations_line
    ; selection_line
    ; selection_value
    ; selection_allocations_line
    ]
;;

let view ~trie ~orientations ~selection ~poi ~focus ~set_focus ~total_allocations =
  let open Vdom in
  let info_view = info_view ~orientations ~selection ~focus ~total_allocations in
  let button_bar = button_bar ~trie ~orientations ~selection ~poi ~focus ~set_focus in
  Panel.panel ~id:"explore-panel" ~title:"Explore" (Node.div [ info_view; button_bar ])
;;

let focus_action ~selection =
  let key = Vdom_keyboard.Keystroke.create' Enter in
  match selection_focus_behavior ~sort:None ~orient:None ~selection with
  | Action act ->
    let keys = [ key ] in
    let description = "Set focus" in
    let group = None in
    let handler _ = act () in
    Vdom_keyboard.Keyboard_event_handler.Action.Command
      { keys; description; group; handler }
  | Disabled -> Disabled_key key
;;

let retract_focus_by_one_action ~selection =
  let key = Vdom_keyboard.Keystroke.create' ~shift:() Enter in
  match retract_focus_by_one_behavior ~selection with
  | Action act ->
    let keys = [ key ] in
    let description = "Zoom out" in
    let group = None in
    let handler _ = act () in
    Vdom_keyboard.Keyboard_event_handler.Action.Command
      { keys; description; group; handler }
  | Disabled -> Disabled_key key
;;

let reset_focus_action ~poi ~focus ~set_focus =
  let key = Vdom_keyboard.Keystroke.create' Escape in
  match reset_focus_behavior ~poi ~focus ~set_focus with
  | Disabled -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
  | Action act ->
    let keys = [ key ] in
    let description = "Reset focus" in
    let group = None in
    let handler _ = act () in
    Command { keys; description; group; handler }
;;

let key_handler ~selection ~poi ~focus ~set_focus =
  Vdom_keyboard.Keyboard_event_handler.of_action_list_exn
    [ focus_action ~selection
    ; retract_focus_by_one_action ~selection
    ; reset_focus_action ~poi ~focus ~set_focus
    ]
;;

let component ~trie ~orientations ~selection ~poi ~focus ~set_focus ~total_allocations =
  let open Bonsai.Let_syntax in
  return
    (let%map trie = trie
     and orientations = orientations
     and selection = selection
     and poi = poi
     and focus = focus
     and set_focus = set_focus
     and total_allocations = total_allocations in
     let view =
       view ~trie ~orientations ~selection ~poi ~focus ~set_focus ~total_allocations
     in
     let key_handler = key_handler ~selection ~poi ~focus ~set_focus in
     { view; key_handler })
;;
