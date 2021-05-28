open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  }

module Orientations = struct
  type t =
    | Both
    | Only of Data.Orientation.t
end

module Which_interface = struct
  type t =
    | Flame_graph
    | Table
end

let can_zoom_to fragment ~orients_visible ~zoom =
  let is_not_current_zoom = not (Data.Fragment.same fragment zoom) in
  let is_not_empty () = not (Data.Fragment.is_empty fragment) in
  let is_dead_end () =
    match orients_visible with
    | Orientations.Only orient ->
      List.is_empty (Data.Fragment.one_frame_extensions ~orient fragment)
    | Both -> List.is_empty (Data.Fragment.all_one_frame_extensions fragment)
  in
  is_not_current_zoom && is_not_empty () && not (is_dead_end ())
;;

type zoom_behavior =
  | Zoom_to of Data.Fragment.t
  | Disabled

let zoom_in_behavior ~orients_visible ~orient ~focus ~zoom =
  let eligible focus =
    let can_zoom =
      can_zoom_to focus.Data.Fragment.Oriented.fragment ~orients_visible ~zoom
    in
    let correct_orientation () = Data.Orientation.equal orient focus.orient in
    let is_extension () =
      Data.Fragment.is_extension zoom ~extension:focus.fragment ~orient ~strictly:true
    in
    can_zoom && correct_orientation () && is_extension ()
  in
  match focus with
  | Some focus when eligible focus -> Zoom_to focus.fragment
  | _ -> Disabled
;;

let zoom_out_behavior ~orients_visible ~orient ~focus ~zoom =
  match focus with
  | Some { Data.Fragment.Oriented.fragment = focus; orient = Callees } ->
    (* Find the focus as *this button* considers it. The button to retract from below
       has to pretend that the focus node *actually* stands for part of the zoom from the
       top, not part of the zoom from the bottom (note that the zoom is considered part
       of the flame graph). *)
    let focus =
      match orient with
      | Data.Orientation.Callees -> Some focus
      | Callers ->
        if Data.Fragment.is_extension focus ~extension:zoom ~orient:Callees ~strictly:true
        then Some (Data.Fragment.flip focus ~extension:zoom ~orient:Callees)
        else None
    in
    let eligible focus =
      let can_zoom = can_zoom_to focus ~orients_visible ~zoom in
      let is_retraction () =
        Data.Fragment.is_extension focus ~extension:zoom ~strictly:true ~orient
      in
      can_zoom && is_retraction ()
    in
    (match focus with
     | Some focus when eligible focus -> Zoom_to focus
     | _ -> Disabled)
  | None
  | Some { orient = Callers; fragment = _ }
    (* The zoom nodes are considered part of the flame graph, so if the focus is in the
       icicle graph, it can't be in the zoom *)
    -> Disabled
;;

let zoom_out_by_one_behavior ~orients_visible ~orient ~zoom =
  match Data.Fragment.retract zoom ~orient with
  | Some new_zoom when can_zoom_to new_zoom ~orients_visible ~zoom -> Zoom_to new_zoom
  | _ -> Disabled
;;

let reset_zoom_behavior ~orients_visible ~poi ~zoom =
  if can_zoom_to poi ~orients_visible ~zoom then Zoom_to poi else Disabled
;;

let refocus_behavior ~trie ~orients_visible ~focus ~zoom =
  match focus with
  | Some focus ->
    let head = Data.Fragment.Oriented.first focus in
    let head_fragment = Data.Fragment_trie.find trie [ head ] |> Option.value_exn in
    if can_zoom_to head_fragment ~orients_visible ~zoom
    then Zoom_to head_fragment
    else Disabled
  | None -> Disabled
;;

let extend_callees_symbol = (* BLACK UP-POINTING TRIANGLE *) "\u{25b2}"
let extend_callers_symbol = (* BLACK DOWN-POINTING TRIANGLE *) "\u{25bc}"
let retract_callees_symbol = extend_callers_symbol
let retract_callers_symbol = extend_callees_symbol

let button_bar
      ~trie
      ~orients_visible
      ~which_interface
      ~focus
      ~poi
      ~set_poi:_
      ~zoom
      ~set_zoom
  =
  let open Vdom in
  let zoom_button ~behavior ~text =
    (* Absolutely loving the fact that Attr.[ ... ] acts like open! and not like open. *)
    let on_click_ =
      match behavior with
      | Zoom_to fragment -> Attr.on_click (fun _ -> set_zoom (Some fragment))
      | Disabled -> Attr.empty
    in
    let disabled_ =
      match behavior with
      | Zoom_to _ -> Attr.empty
      | Disabled -> Attr.disabled
    in
    Node.button Attr.[ on_click_ @ disabled_ ] [ Node.text text ]
  in
  let double_button ~(f : orient:Data.Orientation.t -> Vdom.Node.t) =
    let top_button = f ~orient:Callees in
    let bottom_button = f ~orient:Callers in
    Node.div [ Attr.class_ "double-button" ] [ top_button; bottom_button ]
  in
  (* Having two buttons for zooming in isn't essential; just seems a little clearer to
     have the controls for the flame part and the controls for the icicle part. Also,
     the triangles on the retract buttons are (more) unambiguous this way. *)
  let zoom_in_buttons =
    match orients_visible with
    | Orientations.Only orient ->
      let behavior = zoom_in_behavior ~orients_visible ~focus ~zoom ~orient in
      zoom_button ~behavior ~text:"Zoom in to selected"
    | Both ->
      double_button ~f:(fun ~orient ->
        let behavior = zoom_in_behavior ~orients_visible ~orient ~focus ~zoom in
        let dir_symbol =
          match orient with
          | Callees -> extend_callees_symbol
          | Callers -> extend_callers_symbol
        in
        let text = sprintf "Extend focus %s to selected" dir_symbol in
        zoom_button ~behavior ~text)
  in
  let zoom_out_buttons =
    match which_interface with
    | Which_interface.Flame_graph ->
      (match orients_visible with
       | Only orient ->
         let behavior = zoom_out_behavior ~orients_visible ~focus ~zoom ~orient in
         zoom_button ~behavior ~text:"Zoom out to selected"
       | Both ->
         double_button ~f:(fun ~orient ->
           let behavior = zoom_out_behavior ~orients_visible ~focus ~zoom ~orient in
           let text =
             let dir_symbol =
               match orient with
               | Data.Orientation.Callees -> retract_callees_symbol
               | Callers -> retract_callers_symbol
             in
             sprintf "Retract focus %s to selected" dir_symbol
           in
           zoom_button ~behavior ~text))
    | Table ->
      let orient =
        match orients_visible with
        | Both -> assert false
        | Only orient -> orient
      in
      let behavior = zoom_out_by_one_behavior ~orients_visible ~orient ~zoom in
      let text = "Zoom out" in
      zoom_button ~behavior ~text
  in
  let reset_zoom_button =
    let behavior = reset_zoom_behavior ~orients_visible ~poi ~zoom in
    let text =
      match orients_visible with
      | Only _ -> "Reset zoom to point of interest"
      | Both -> "Reset focus to point of interest"
    in
    zoom_button ~behavior ~text
  in
  let refocus_button =
    let behavior = refocus_behavior ~trie ~orients_visible ~focus ~zoom in
    let text = "Focus on selected node only" in
    zoom_button ~behavior ~text
  in
  Node.div
    [ Attr.class_ "button-bar" ]
    [ zoom_in_buttons; zoom_out_buttons; reset_zoom_button; refocus_button ]
;;

(* sequestering these here to isolate the confusing bits *)
let zoom_label ~orients_visible =
  match orients_visible with
  | Orientations.Only _ ->
    (* This is a table or just the flame graph or icicle graph, so buttons like "zoom in"
       make the most sense *)
    "Zoomed"
  | Both ->
    (* This is a double flame graph, so the zoom metaphor breaks down *)
    "Focused"
;;

let focus_label = "Selected"
let en_dash = "\u{2013}"

let info_view ~orients_visible ~focus ~zoom ~total_allocations =
  let open Vdom in
  let line ?(attrs = Attr.empty) label nodes =
    Node.p
      []
      (List.concat
         [ [ Node.span Attr.[ class_ "info-label" @ attrs ] [ Node.textf "%s: " label ] ]
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
  let zoom_line = line (zoom_label ~orients_visible) [] in
  let zoom_value = Backtrace_view.render (Data.Fragment.backtrace zoom) in
  let zoom_allocations_line =
    let fragment =
      zoom
      |> Option.some_if (not (Data.Backtrace.is_trivial (Data.Fragment.backtrace zoom)))
    in
    allocations_line "Total allocations" ~fragment
  in
  let focus_line = line focus_label [] in
  let focus_value =
    let contents =
      match focus with
      | Some { Data.Fragment.Oriented.fragment; orient } ->
        Location.format_dom (Data.Fragment.first fragment ~orient)
      | None -> Node.span [ Attr.class_ "loc-special" ] [ Node.text "(none)" ]
    in
    Node.p [ Attr.class_ "indented" ] [ contents ]
  in
  let focus_allocations_line =
    allocations_line
      "Allocations"
      ~fragment:
        (focus
         |> Option.map ~f:(fun { Data.Fragment.Oriented.fragment; orient = _ } -> fragment)
        )
  in
  Node.div
    []
    [ zoom_line
    ; zoom_value
    ; zoom_allocations_line
    ; focus_line
    ; focus_value
    ; focus_allocations_line
    ]
;;

let view
      ~trie
      ~orients_visible
      ~which_interface
      ~focus
      ~poi
      ~set_poi
      ~zoom
      ~set_zoom
      ~total_allocations
  =
  let open Vdom in
  let info_view = info_view ~orients_visible ~focus ~zoom ~total_allocations in
  let button_bar =
    button_bar
      ~trie
      ~orients_visible
      ~which_interface
      ~focus
      ~poi
      ~set_poi
      ~zoom
      ~set_zoom
  in
  Panel.panel ~id:"zoom-panel" ~title:"Explore" (Node.div [] [ info_view; button_bar ])
;;

let zoom_action ~orients_visible ~focus ~zoom ~set_zoom =
  let key = Vdom_keyboard.Keystroke.create' Enter in
  let command fragment =
    let keys = [ key ] in
    let description = "Set zoom" in
    let group = None in
    let handler _ = set_zoom (Some fragment) in
    Vdom_keyboard.Keyboard_event_handler.Action.Command
      { keys; description; group; handler }
  in
  let rec first_enabled_behavior = function
    | [] -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
    | Zoom_to fragment :: _ -> command fragment
    | Disabled :: behaviors -> first_enabled_behavior behaviors
  in
  first_enabled_behavior
    [ zoom_in_behavior ~orients_visible ~focus ~zoom ~orient:Callees
    ; zoom_in_behavior ~orients_visible ~focus ~zoom ~orient:Callers
    ; zoom_out_behavior ~orients_visible ~focus ~zoom ~orient:Callees
    ; zoom_out_behavior ~orients_visible ~focus ~zoom ~orient:Callers
    ]
;;

let zoom_out_by_one_action ~orients_visible ~zoom ~set_zoom =
  let key = Vdom_keyboard.Keystroke.create' ~shift:() Enter in
  let command fragment =
    let keys = [ key ] in
    let description = "Zoom out" in
    let group = None in
    let handler _ = set_zoom (Some fragment) in
    Vdom_keyboard.Keyboard_event_handler.Action.Command
      { keys; description; group; handler }
  in
  match orients_visible with
  | Orientations.Both -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
  | Only orient ->
    (match zoom_out_by_one_behavior ~orients_visible ~orient ~zoom with
     | Zoom_to fragment -> command fragment
     | Disabled -> Disabled_key key)
;;

let reset_zoom_action ~orients_visible ~poi ~zoom ~set_zoom =
  let key = Vdom_keyboard.Keystroke.create' Escape in
  match reset_zoom_behavior ~orients_visible ~poi ~zoom with
  | Disabled -> Vdom_keyboard.Keyboard_event_handler.Action.Disabled_key key
  | Zoom_to _ ->
    let keys = [ key ] in
    let description = "Reset zoom" in
    let group = None in
    let handler _ = set_zoom None in
    Command { keys; description; group; handler }
;;

let key_handler ~orients_visible ~focus ~poi ~zoom ~set_zoom =
  Vdom_keyboard.Keyboard_event_handler.of_action_list_exn
    [ zoom_action ~orients_visible ~focus ~zoom ~set_zoom
    ; zoom_out_by_one_action ~orients_visible ~zoom ~set_zoom
    ; reset_zoom_action ~orients_visible ~poi ~zoom ~set_zoom
    ]
;;

let component
      ~trie
      ~orients_visible
      ~which_interface
      ~focus
      ~poi
      ~set_poi
      ~zoom
      ~set_zoom
      ~total_allocations
  =
  let open Bonsai.Let_syntax in
  return
    (let%map trie = trie
     and orients_visible = orients_visible
     and which_interface = which_interface
     and focus = focus
     and poi = poi
     and set_poi = set_poi
     and zoom = zoom
     and set_zoom = set_zoom
     and total_allocations = total_allocations in
     let view =
       view
         ~trie
         ~orients_visible
         ~which_interface
         ~focus
         ~poi
         ~set_poi
         ~zoom
         ~set_zoom
         ~total_allocations
     in
     let key_handler = key_handler ~orients_visible ~focus ~poi ~zoom ~set_zoom in
     { view; key_handler })
;;
