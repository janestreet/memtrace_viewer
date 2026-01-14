open! Core
open Bonsai_web_proc
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr
include Flame_graph_view_intf

let node_height = 20.
let control_area_width = 63.
let focus_border_thickness = 4. (* needs to match CSS .flame-graph-sequence-border *)
let focus_border_protrusion = 14.
let button_width = 8.
let button_height = button_width /. Float.sqrt 2.
let button_margin = 3.
let bracket_margin = 3.
let bracket_arm_length = 3.
let bracket_tick_length = 3.

module Controls = struct
  module Glyph = struct
    type t =
      | Up_to_here
      | Down_to_here

    let render t x y =
      let points =
        match t with
        | Up_to_here ->
          [ x +. (0.5 *. button_width), y
          ; x, y +. button_height
          ; x +. button_width, y +. button_height
          ]
        | Down_to_here ->
          [ x, y; x +. button_width, y; x +. (0.5 *. button_width), y +. button_height ]
      in
      Node_svg.polygon
        ~attrs:[ Attr_svg.points points; Vdom.Attr.class_ "flame-graph-button-glyph" ]
        []
    ;;
  end

  let render_motion_line ~x ~y ~arrow_base_y =
    let line_x = x +. (button_width /. 2.) in
    let end_y =
      if Float.(arrow_base_y > y)
      then (* Don't smudge the point of the arrow *)
        y +. button_height
      else y
    in
    Node_svg.line
      ~attrs:
        [ Vdom.Attr.class_ "flame-graph-button-motion-line"
        ; Attr_svg.x1 line_x
        ; Attr_svg.y1 arrow_base_y
        ; Attr_svg.x2 line_x
        ; Attr_svg.y2 end_y
        ]
      []
  ;;

  let render_button ~glyph ~x ~y ~arrow_base_y ~on_click =
    let on_click _ = on_click in
    Node_svg.g
      ~attrs:[ Vdom.Attr.class_ "flame-graph-button"; Vdom.Attr.on_click on_click ]
      [ Glyph.render glyph x y; render_motion_line ~x ~y ~arrow_base_y ]
  ;;
end

module Style = struct
  type t =
    | Flames
    | Focus
    | Icicles

  let random_flame_color () =
    let v1 = Random.float 1. in
    let v2 = Random.float 1. in
    let v3 = Random.float 1. in
    let r = 205 + (50. *. v3 |> Int.of_float) in
    let g = 0 + (230. *. v1 |> Int.of_float) in
    let b = 0 + (55. *. v2 |> Int.of_float) in
    `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ())
  ;;

  let random_icicle_color () =
    let v1 = Random.float 1. in
    let v2 = Random.float 1. in
    let v3 = Random.float 1. in
    let r = 50 + (55. *. v2 |> Int.of_float) in
    let g = 100 + (130. *. v1 |> Int.of_float) in
    let b = 230 + (25. *. v3 |> Int.of_float) in
    `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ())
  ;;

  let focus_color_odd : Css_gen.Color.t = `Var "--focus-color-odd"
  let focus_color_even = `Var "--focus-color-even"
  let focus_color even = if even then focus_color_even else focus_color_odd

  let get_color even = function
    | Flames -> random_flame_color ()
    | Icicles -> random_icicle_color ()
    | Focus -> focus_color even
  ;;

  let get_node_css even style =
    let color = get_color even style in
    Css_gen.create_with_color ~field:"fill" ~color
  ;;
end

module Bearing = struct
  type t =
    | Upwards
    | Downwards

  let succ_y ~bearing y =
    match bearing with
    | Upwards -> y -. node_height
    | Downwards -> y +. node_height
  ;;
end

module Make (Graph : Graph) = struct
  module Selector = struct
    type t =
      | Flame of Graph.Tree.t
      | Icicle of Graph.Tree.t
      | Focus of Graph.Sequence.t

    let flame tree = Flame tree
    let icicle tree = Icicle tree
    let focus seq = Focus seq

    let same t1 t2 =
      match t1, t2 with
      | Flame tree1, Flame tree2 -> Graph.Tree.same tree1 tree2
      | Flame _, (Icicle _ | Focus _) -> false
      | Icicle tree1, Icicle tree2 -> Graph.Tree.same tree1 tree2
      | Icicle _, (Flame _ | Focus _) -> false
      | Focus seq1, Focus seq2 -> Graph.Sequence.same seq1 seq2
      | Focus _, (Flame _ | Icicle _) -> false
    ;;
  end

  module Context = struct
    (* Parameters that are unchanged throughout the rendering of one section (the sections
       being flame, icicle, and focus). *)
    type t =
      { graph : Graph.t
      ; selection : Selector.t option
      ; select : Selector.t -> unit Effect.t
      ; activate : Selector.t -> unit Effect.t
      ; commands : Commands.t
      ; style : Style.t
      ; bearing : Bearing.t
      ; data_left : float
      ; focus_border_above : float option
          (* y-coordinate of the focus border above the section *)
      ; focus_border_below : float option
      }
  end

  let render_buttons ~cxt ~y ~selected =
    let Commands.
          { extend_focus_to; retract_callers_from_focus; retract_callees_from_focus }
      =
      cxt.Context.commands
    in
    if not selected
    then []
    else
      let open struct
        type position =
          (* Bottom aligned with top edge of node *)
          | Over_border
          | (* Top aligned with top edge of node *)
            Up_to_border
          | (* Bottom aligned with bottom edge of node *)
            Down_to_border
          | (* Top aligned with bottom edge of node *)
            Under_border
      end in
      let button glyph vpos command =
        match (command : Command.t) with
        | Enabled on_click ->
          let x = cxt.data_left -. (button_margin +. button_width) in
          let y =
            match vpos with
            | Over_border -> y -. button_width
            | Up_to_border -> y
            | Down_to_border -> y +. (node_height -. button_width)
            | Under_border -> y +. node_height
          in
          let arrow_base_y =
            match (glyph : Controls.Glyph.t) with
            | Down_to_here -> cxt.Context.focus_border_above |> Option.value_exn
            | Up_to_here -> cxt.focus_border_below |> Option.value_exn
          in
          Some (Controls.render_button ~glyph ~x ~y ~arrow_base_y ~on_click)
        | Disabled -> None
      in
      let extend_focus_to_button =
        let (glyph : Controls.Glyph.t), vpos =
          match cxt.style with
          | Flames | Focus -> Up_to_here, Up_to_border
          | Icicles -> Down_to_here, Down_to_border
        in
        button glyph vpos extend_focus_to
      in
      let retract_callers_from_focus_button =
        button Up_to_here Under_border retract_callers_from_focus
      in
      let retract_callees_from_focus_button =
        button Down_to_here Over_border retract_callees_from_focus
      in
      List.filter_opt
        [ extend_focus_to_button
        ; retract_callers_from_focus_button
        ; retract_callees_from_focus_button
        ]
  ;;

  let color_box_class_attr = Vdom.Attr.class_ "flame-graph-node-color-box"
  let height_attr = Attr_svg.height node_height
  let text_x_attr = Attr_svg.x 2.
  let text_y_attr = Attr_svg.y (0.8 *. node_height)

  let render_node ~cxt ~x ~y ~width ~even ~selector node : Vdom.Node.t list =
    let graph = cxt.Context.graph in
    let css = Style.get_node_css even cxt.style in
    let selected = Option.exists ~f:(Selector.same selector) cxt.selection in
    let node_type_class =
      match Graph.Node.type_ ~graph node with
      | Function -> Vdom.Attr.class_ "flame-graph-node-function"
      | Allocation_site -> Vdom.Attr.class_ "flame-graph-node-alloc-site"
    in
    let selected_class =
      if selected then Vdom.Attr.class_ "flame-graph-node-selected" else Vdom.Attr.empty
    in
    let alloc_site_indicator =
      match Graph.Node.type_ ~graph node with
      | Function -> Vdom.Node.none_deprecated [@alert "-deprecated"]
      | Allocation_site ->
        Node_svg.tspan
          ~attrs:[ Vdom.Attr.class_ "loc-alloc-site-indicator" ]
          [ Vdom.Node.text "A " ]
    in
    let buttons = render_buttons ~cxt ~y ~selected in
    let rect =
      (* Put the node in an embedded <svg> element so that the text gets clipped to the
         rectangle *)
      Node_svg.svg
        ~attrs:
          [ Attr_svg.x x
          ; Attr_svg.y y
          ; Attr_svg.width width
          ; height_attr
          ; Vdom.Attr.classes [ "flame-graph-node" ]
          ; node_type_class
          ; selected_class
          ; Vdom.Attr.on_click (fun _ -> cxt.select selector)
          ; Vdom.Attr.on_double_click (fun _ -> cxt.activate selector)
          ]
        [ Node_svg.rect
            ~attrs:
              [ color_box_class_attr
              ; Vdom.Attr.style css
              ; (* Since we set the width on the svg element above, we should be able to
                   let CSS take care of setting this rect's width to 100% of its container
                   and be done with it, but Chrome was sporadically making the rect very
                   small for no apparent reason. Height doesn't seem to have the same
                   problem. *)
                Attr_svg.width width
              ]
            []
        ; Node_svg.text
            ~attrs:[ text_x_attr; text_y_attr ]
            [ alloc_site_indicator; Vdom.Node.text (Graph.Node.label ~graph node) ]
        ; Node_svg.title [ Vdom.Node.text (Graph.Node.details ~graph node) ]
        ]
    in
    List.concat [ buttons; [ rect ] ]
  ;;

  let render_sequence ~cxt ~x ~y ~width ~make_selector seq : Vdom.Node.t list =
    let graph = cxt.Context.graph in
    let bearing = cxt.bearing in
    let starting_y = y in
    let y = ref starting_y in
    let rec render_nodes ~even seq =
      let first = Graph.Sequence.node seq in
      let first_view =
        let selector = make_selector seq in
        render_node ~cxt ~x ~y:!y ~width ~even ~selector first
      in
      let rest_view =
        match Graph.Sequence.next ~graph seq with
        | None -> []
        | Some rest ->
          y := Bearing.succ_y ~bearing !y;
          let even = not even in
          render_nodes ~even rest
      in
      first_view :: rest_view
    in
    let nodes =
      match seq with
      | None -> []
      | Some seq -> List.concat (render_nodes ~even:false seq)
    in
    let border y =
      let segment ?(attr = Vdom.Attr.empty) x1 x2 =
        Node_svg.line
          ~attrs:
            [ attr
            ; Attr_svg.x1 x1
            ; Attr_svg.y1 y
            ; Attr_svg.x2 x2
            ; Attr_svg.y2 y
            ; Vdom.Attr.class_ "flame-graph-sequence-border"
            ]
          []
      in
      [ segment
          (x -. focus_border_protrusion)
          (x +. (width /. 2.))
          ~attr:(Attr_svg.stroke_linecap `Round)
      ; segment (x +. (width /. 2.)) (x +. width)
      ]
    in
    let starting_border_y, ending_border_y =
      match bearing with
      | Upwards -> starting_y +. node_height, !y
      | Downwards -> starting_y, !y +. node_height
    in
    let starting_border = border starting_border_y in
    let ending_border =
      match seq with
      | None -> []
      | Some _ -> border ending_border_y
    in
    let bracket_right_x = x -. focus_border_protrusion -. bracket_margin in
    let bracket =
      match seq with
      | None -> []
      | Some seq ->
        let right_x = bracket_right_x in
        let middle_x = bracket_right_x -. bracket_arm_length in
        let left_x = middle_x -. bracket_tick_length in
        let line =
          Node_svg.polyline
            ~attrs:
              [ Attr_svg.points
                  [ right_x, starting_border_y
                  ; middle_x, starting_border_y
                  ; middle_x, ending_border_y
                  ; right_x, ending_border_y
                  ]
              ]
            []
        in
        let middle_y = (starting_border_y +. ending_border_y) /. 2. in
        let tick =
          Node_svg.polyline
            ~attrs:[ Attr_svg.points [ middle_x, middle_y; left_x, middle_y ] ]
            []
        in
        let text_right_x = left_x -. bracket_margin in
        let text_lines =
          let first = ref true in
          List.map (Graph.Sequence.label ~graph seq) ~f:(fun line ->
            let dy = if !first then Vdom.Attr.empty else Vdom.Attr.create "dy" "1.2em" in
            first := false;
            Node_svg.tspan
              ~attrs:[ Attr_svg.x text_right_x; Attr_svg.y (middle_y -. 3.); dy ]
              [ Vdom.Node.text line ])
        in
        let text =
          Node_svg.text ~attrs:[ Vdom.Attr.create "text-anchor" "end" ] text_lines
        in
        [ Node_svg.g
            ~attrs:[ Vdom.Attr.class_ "flame-graph-sequence-bracket" ]
            [ line; tick; text ]
        ]
    in
    (* An invisible box that's there so we can scroll the whole sequence into view (see
       [scroll_focus_into_view] below) *)
    let box =
      match seq with
      | None -> []
      | Some _ ->
        let height = Float.abs (starting_border_y -. ending_border_y) in
        let y = Float.min starting_border_y ending_border_y in
        [ Node_svg.rect
            ~attrs:
              [ Attr_svg.x x
              ; Attr_svg.y y
              ; Attr_svg.width width
              ; Attr_svg.height height
              ; Vdom.Attr.class_ "flame-graph-sequence-box"
              ]
            []
        ]
    in
    List.concat [ box; nodes; starting_border; ending_border; bracket ]
  ;;

  let rec render_tree ~cxt ~x ~y ~scale ~even ~make_selector tree : Vdom.Node.t list =
    let graph = cxt.Context.graph in
    let bearing = cxt.bearing in
    let node = Graph.Tree.node tree in
    let width = Graph.Node.size ~graph node *. scale in
    let view =
      let selector = make_selector tree in
      render_node ~cxt ~x ~y ~width ~even ~selector node
    in
    let children_view =
      let even = not even in
      let y = Bearing.succ_y ~bearing y in
      let children = Graph.Tree.children ~graph tree in
      let children_size =
        List.fold_left children ~init:0. ~f:(fun total child ->
          total +. Graph.Node.size ~graph (Graph.Tree.node child))
      in
      let children_width = children_size *. scale in
      let x = x +. ((width -. children_width) /. 2.) in
      render_level ~cxt ~x ~y ~scale ~even ~make_selector children
    in
    List.concat [ view; children_view ]

  and render_level ~cxt ~x ~y ~scale ~even ~make_selector trees =
    let graph = cxt.Context.graph in
    let x = ref x in
    let tree_views =
      List.concat_map trees ~f:(fun tree ->
        let this_x = !x in
        let node = Graph.Tree.node tree in
        let width = Graph.Node.size ~graph node *. scale in
        x := !x +. width;
        render_tree ~cxt ~x:this_x ~y ~scale ~even ~make_selector tree)
    in
    (* If there's a single child, flatten the DOM out a little; if nothing else, it makes
       it easier to poke around using the Chrome inspector *)
    match trees with
    | [] | [ _ ] -> tree_views
    | _ -> [ Node_svg.g tree_views ]
  ;;

  let rec tree_height ~graph trees =
    List.fold_left trees ~init:0 ~f:(fun acc tree ->
      let height = 1 + tree_height ~graph (Graph.Tree.children ~graph tree) in
      Int.max acc height)
  ;;

  let rec sequence_beginning ~graph seq =
    match Graph.Sequence.prev ~graph seq with
    | None -> seq
    | Some prev -> sequence_beginning ~graph prev
  ;;

  let rec sequence_end ~graph seq =
    match Graph.Sequence.next ~graph seq with
    | None -> seq
    | Some next -> sequence_end ~graph next
  ;;

  let rec sequence_height ~graph seq =
    match Graph.Sequence.next ~graph seq with
    | None -> 1
    | Some next -> 1 + sequence_height ~graph next
  ;;

  let render ~width ~set_width ~selection ~select ~activate ~commands graph =
    Random.init 0;
    let focus = Option.map ~f:(sequence_beginning ~graph) (Graph.focus graph) in
    let actual_control_area_width =
      match focus with
      | None ->
        (* If there's no focus, the control area is just a bit of space for the buttons *)
        focus_border_protrusion
      | Some _ ->
        (* If there's a focus, reserve room for the bracket and the focused allocations *)
        control_area_width
    in
    let data_left = 0. +. actual_control_area_width in
    let data_width = width -. actual_control_area_width in
    let flame_tree = Graph.flame_tree graph in
    let icicle_tree = Graph.icicle_tree graph in
    let size = Graph.size graph in
    let scale = data_width /. size in
    (* If the focus border is at the very top (because there's no flame tree), it needs a
       bit of extra space so it doesn't get clipped *)
    let extra_space_at_top_for_border =
      if List.is_empty flame_tree then focus_border_thickness /. 2. else 0.
    in
    let flame_height = node_height *. (tree_height ~graph flame_tree |> Float.of_int) in
    let focus_height =
      match focus with
      | None -> 0.
      | Some seq -> node_height *. (sequence_height ~graph seq |> Float.of_int)
    in
    let icicle_height = node_height *. (tree_height ~graph icicle_tree |> Float.of_int) in
    let extra_space_at_bottom_for_border =
      if List.is_empty icicle_tree then focus_border_thickness /. 2. else 0.
    in
    let height =
      extra_space_at_top_for_border
      +. flame_height
      +. focus_height
      +. icicle_height
      +. extra_space_at_bottom_for_border
    in
    let flame_graph_bottom_node_y =
      flame_height -. node_height +. extra_space_at_top_for_border
    in
    let focus_top_node_y = flame_graph_bottom_node_y +. node_height in
    let focus_bottom_node_y = flame_graph_bottom_node_y +. focus_height in
    let icicle_graph_top_node_y = focus_bottom_node_y +. node_height in
    let flame_graph_views =
      render_level
        ~cxt:
          { graph
          ; style = Flames
          ; bearing = Upwards
          ; selection
          ; select
          ; activate
          ; commands
          ; data_left
          ; focus_border_above = None
          ; focus_border_below = Some focus_top_node_y
          }
        ~x:data_left
        ~y:flame_graph_bottom_node_y
        ~scale
        ~even:false
        ~make_selector:Selector.flame
        flame_tree
    in
    let focus_views =
      render_sequence
        ~cxt:
          { graph
          ; style = Focus
          ; bearing = Upwards
          ; selection
          ; select
          ; activate
          ; commands
          ; data_left
          ; focus_border_above = Some focus_top_node_y
          ; focus_border_below = Some (focus_bottom_node_y +. node_height)
          }
        ~x:data_left
        ~y:focus_bottom_node_y
        ~width:data_width
        ~make_selector:Selector.focus
        focus
    in
    let icicle_graph_views =
      render_level
        ~cxt:
          { graph
          ; style = Icicles
          ; bearing = Downwards
          ; selection
          ; select
          ; activate
          ; commands
          ; data_left
          ; focus_border_below = None
          ; focus_border_above = Some (focus_bottom_node_y +. node_height)
          }
        ~x:data_left
        ~y:icicle_graph_top_node_y
        ~scale
        ~even:false
        ~make_selector:Selector.icicle
        icicle_tree
    in
    let open Vdom in
    let flame_view =
      Node_svg.g ~attrs:[ Attr.class_ "flame-graph-flames" ] flame_graph_views
    in
    let focus_view = Node_svg.g ~attrs:[ Attr.class_ "flame-graph-focus" ] focus_views in
    let icicle_view =
      Node_svg.g ~attrs:[ Attr.class_ "flame-graph-icicles" ] icicle_graph_views
    in
    let on_size_change =
      Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
        (fun { border_box = { width; height = _ }; content_box = _ } -> set_width width)
    in
    Node.div
      ~attrs:[ Attr.class_ "flame-graph-container" ]
      [ Node.div
          ~attrs:[ Attr.class_ "flame-graph-sizer"; on_size_change ]
          [ Node_svg.svg
              ~attrs:
                [ Attr_svg.height height
                ; Attr_svg.viewbox ~min_x:0. ~min_y:0. ~width ~height
                ]
              [ flame_view; icicle_view; focus_view ]
          ]
      ]
  ;;

  module Keyboard_navigation = struct
    let to_parent ~graph tree = Graph.Tree.parent ~graph tree

    let largest_tree ~graph trees =
      List.max_elt trees ~compare:(fun tree1 tree2 ->
        let node1 = Graph.Tree.node tree1 in
        let node2 = Graph.Tree.node tree2 in
        let size1 = Graph.Node.size ~graph node1 in
        let size2 = Graph.Node.size ~graph node2 in
        Float.compare size1 size2)
    ;;

    let to_child ~graph tree =
      let children = Graph.Tree.children ~graph tree in
      largest_tree ~graph children
    ;;

    type left_or_right =
      | Left
      | Right

    let to_sibling ~graph ~origin ~which tree =
      let siblings =
        match Graph.Tree.parent ~graph tree with
        | None -> origin
        | Some parent -> Graph.Tree.children ~graph parent
      in
      let rec find_successor trees =
        match trees with
        | first_tree :: (next_tree :: _ as rest) ->
          if Graph.Tree.same first_tree tree then Some next_tree else find_successor rest
        | [ _ ] -> None
        | [] -> None
      in
      let siblings_oriented =
        match which with
        | Right -> siblings
        | Left -> List.rev siblings
      in
      find_successor siblings_oriented
    ;;

    let move_into_flame_tree ~graph =
      let%map.Option largest_flame_tree = largest_tree ~graph (Graph.flame_tree graph) in
      Selector.Flame largest_flame_tree
    ;;

    let move_into_icicle_tree ~graph =
      let%map.Option largest_icicle_tree =
        largest_tree ~graph (Graph.icicle_tree graph)
      in
      Selector.Icicle largest_icicle_tree
    ;;

    let move_down_to_focus ~graph =
      match Graph.focus graph with
      | Some seq -> Some (Selector.Focus (sequence_end ~graph seq))
      | None -> move_into_icicle_tree ~graph
    ;;

    let move_up_to_focus ~graph =
      match Graph.focus graph with
      | Some seq -> Some (Selector.Focus (sequence_beginning ~graph seq))
      | None -> move_into_flame_tree ~graph
    ;;

    let move_flame_tree ~graph ~(dir : Direction.t) tree =
      let origin = Graph.flame_tree graph in
      match dir with
      | Up ->
        let%map.Option new_tree = to_child ~graph tree in
        Selector.Flame new_tree
      | Down ->
        (match to_parent ~graph tree with
         | Some new_tree -> Some (Selector.Flame new_tree)
         | None -> move_down_to_focus ~graph)
      | Left ->
        let%map.Option new_tree = to_sibling ~origin ~graph ~which:Left tree in
        Selector.Flame new_tree
      | Right ->
        let%map.Option new_tree = to_sibling ~origin ~graph ~which:Right tree in
        Selector.Flame new_tree
    ;;

    let move_icicle_tree ~graph ~(dir : Direction.t) tree =
      let origin = Graph.icicle_tree graph in
      match dir with
      | Down ->
        let%map.Option new_tree = to_child ~graph tree in
        Selector.Icicle new_tree
      | Up ->
        (match to_parent ~graph tree with
         | Some new_tree -> Some (Selector.Icicle new_tree)
         | None -> move_up_to_focus ~graph)
      | Left ->
        let%map.Option new_tree = to_sibling ~origin ~graph ~which:Left tree in
        Selector.Icicle new_tree
      | Right ->
        let%map.Option new_tree = to_sibling ~origin ~graph ~which:Right tree in
        Selector.Icicle new_tree
    ;;

    let move_focus ~graph ~(dir : Direction.t) seq =
      match dir with
      | Up ->
        (match Graph.Sequence.next ~graph seq with
         | Some seq -> Some (Selector.Focus seq)
         | None -> move_into_flame_tree ~graph)
      | Down ->
        (match Graph.Sequence.prev ~graph seq with
         | Some seq -> Some (Selector.Focus seq)
         | None -> move_into_icicle_tree ~graph)
      | Left -> None
      | Right -> None
    ;;

    let move ~graph ~dir (selector : Selector.t option) =
      match selector with
      | Some (Flame tree) -> move_flame_tree ~graph ~dir tree
      | Some (Icicle tree) -> move_icicle_tree ~graph ~dir tree
      | Some (Focus seq) -> move_focus ~graph ~dir seq
      | None ->
        (match dir with
         | Direction.Up -> move_into_flame_tree ~graph
         | Direction.Down -> move_into_icicle_tree ~graph
         | Direction.Left | Direction.Right -> None)
    ;;

    let move_event ~graph ~dir ~selection ~navigate_to =
      let new_selection = move ~graph ~dir selection in
      match new_selection with
      | Some new_selection ->
        Vdom.Effect.Many [ Vdom.Effect.Prevent_default; navigate_to new_selection ]
      | None -> Vdom.Effect.Ignore
    ;;

    let handle_arrow_key ~graph ~selection ~navigate_to event =
      let dir : Direction.t =
        match Vdom_keyboard.Keyboard_event.key event with
        | ArrowUp | KeyK -> Up
        | ArrowDown | KeyJ -> Down
        | ArrowLeft | KeyH -> Left
        | ArrowRight | KeyL -> Right
        | _ -> assert false
      in
      move_event ~dir ~graph ~selection ~navigate_to
    ;;

    let arrow_key_action ~graph ~selection ~navigate_to =
      let open Vdom_keyboard in
      let keys =
        List.map
          ~f:Keystroke.create'
          [ Keyboard_event.Keyboard_code.ArrowUp
          ; KeyK
          ; ArrowDown
          ; KeyJ
          ; ArrowLeft
          ; KeyH
          ; ArrowRight
          ; KeyL
          ]
      in
      let description = "Move selection" in
      let group = None in
      let handler = handle_arrow_key ~graph ~selection ~navigate_to in
      Keyboard_event_handler.Action.Command { keys; description; group; handler }
    ;;

    let enter_action ~selection ~activate =
      let open Vdom_keyboard in
      let enter = Keystroke.create' Enter in
      match selection with
      | None -> Keyboard_event_handler.Action.Disabled_key enter
      | Some selection ->
        let keys = [ enter ] in
        let description = "Extend focus" in
        let group = None in
        let handler _ = activate selection in
        Command { keys; description; group; handler }
    ;;

    let key_handler ~graph ~selection ~navigate_to ~activate =
      Vdom_keyboard.Keyboard_event_handler.of_action_list_exn
        [ arrow_key_action ~graph ~selection ~navigate_to
        ; enter_action ~selection ~activate
        ]
    ;;
  end

  let scroll_into_view : string -> unit Effect.t =
    let open Js_of_ocaml in
    Effect.of_sync_fun (fun selector ->
      let selected_node = Dom_html.document##querySelector (selector |> Js.string) in
      Js.Opt.iter selected_node (fun selected ->
        let options =
          object%js (self)
            val block = "nearest" |> Js.string
            val inline = "nearest" |> Js.string
          end
        in
        Js.Unsafe.meth_call selected "scrollIntoView" [| Js.Unsafe.inject options |]))
  ;;

  let scroll_selection_into_view =
    scroll_into_view ".flame-graph-node-selected .flame-graph-node-color-box"
  ;;

  let scroll_focus_into_view =
    scroll_into_view ".flame-graph-focus .flame-graph-sequence-box"
  ;;

  type t =
    { view : Vdom.Node.t
    ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
    }

  let component graph ~selection ~select ~navigate_to ~activate ~commands =
    let open Bonsai.Let_syntax in
    let%sub width, set_width = Bonsai.state 500. ~equal:[%equal: Float.t] in
    return
      (let%map graph
       and width
       and set_width
       and selection
       and select
       and navigate_to
       and activate
       and commands in
       let view = render ~width ~set_width ~selection ~select ~activate ~commands graph in
       let key_handler =
         Keyboard_navigation.key_handler ~graph ~selection ~navigate_to ~activate
       in
       { view; key_handler })
  ;;
end
