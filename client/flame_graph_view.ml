open! Core_kernel
open Bonsai_web
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr
include Flame_graph_view_intf


let node_height = 20.

module Make (Tree : Tree) = struct
  module Node = struct
    type t =
      { tree_node : Tree.Node.t
      ; shape : Shape.t
      }

    let parent { tree_node; shape } =
      Tree.Node.parent ~shape tree_node
      |> Option.map ~f:(fun tree_node -> { tree_node; shape })
    ;;

    let children { tree_node; shape } =
      Tree.Node.children ~shape tree_node
      |> List.map ~f:(fun tree_node -> { tree_node; shape })
    ;;

    module Debug = struct
      type nonrec t = t

      let sexp_of_t { tree_node; shape } =
        [%message "" ~tree_node:(Tree.Node.label ~shape tree_node) (shape : Shape.t)]
      ;;
    end
  end

  let color_for_node ~shape (_node : _) =
    (* Based on the random algorithm from the original flamegraph.pl *)
    let v1 = Random.float 1. in
    let v2 = Random.float 1. in
    let v3 = Random.float 1. in
    let r, g, b =
      match shape with
      | Shape.Flames ->
        let r = 205 + (50. *. v3 |> Int.of_float) in
        let g = 0 + (230. *. v1 |> Int.of_float) in
        let b = 0 + (55. *. v2 |> Int.of_float) in
        r, g, b
      | Shape.Icicles ->
        let r = 50 + (55. *. v2 |> Int.of_float) in
        let g = 100 + (130. *. v1 |> Int.of_float) in
        let b = 230 + (25. *. v3 |> Int.of_float) in
        r, g, b
    in
    `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ())
  ;;

  let rec render_node
            ~x
            ~y
            ~width
            ~shape
            ~focus
            ~set_focus
            ~in_zoom
            ~zoom_nodes
            ~set_zoom
            ~even
            node
    : Vdom.Node.t list
    =
    let style =
      let color = color_for_node ~shape node |> Css_gen.Color.to_string_css in
      Css_gen.create ~field:"fill" ~value:color
      |> Css_gen.combine (Css_gen.create ~field:"--foo" ~value:"red")
    in
    let children =
      let even = not even in
      let y =
        if Tree.Node.hidden ~shape node
        then y
        else (
          match shape with
          | Flames -> y -. node_height
          | Icicles -> y +. node_height)
      in
      match zoom_nodes with
      | next_zoom_node :: zoom_nodes ->
        let child_nodes = [ next_zoom_node ] in
        let in_zoom = true in
        let size = Tree.Node.size next_zoom_node in
        render_level
          ~x
          ~y
          ~width
          ~shape
          ~size
          ~focus
          ~set_focus
          ~in_zoom
          ~zoom_nodes
          ~set_zoom
          ~even
          child_nodes
      | [] ->
        let child_nodes = Tree.Node.children ~shape node in
        let size = Tree.Node.size node in
        let children_size =
          List.fold_left child_nodes ~init:0. ~f:(fun total child ->
            total +. Tree.Node.size child)
        in
        let children_width = width *. (children_size /. size) in
        let x = x +. ((width -. children_width) /. 2.) in
        let width = children_width in
        let in_zoom = false in
        render_level
          ~x
          ~y
          ~width
          ~shape
          ~size
          ~focus
          ~set_focus
          ~in_zoom
          ~zoom_nodes
          ~set_zoom
          ~even
          child_nodes
    in
    let view =
      if Tree.Node.hidden ~shape node
      then None
      else (
        let focused =
          match focus with
          | Some focus ->
            Shape.equal focus.Node.shape shape && Tree.Node.same focus.tree_node node
          | None -> false
        in
        let classes =
          List.concat
            [ [ "flame-graph-node" ]
            ; (if focused then [ "flame-graph-node-focused" ] else [])
            ; (if in_zoom then [ "flame-graph-node-zoomed-into" ] else [])
            ; (if even then [ "even" ] else [])
            ]
        in
        Some
          ((* Put the node in an embedded <svg> element so that the text gets clipped to the
              rectangle *)
            Node_svg.svg
              [ Attr_svg.x x
              ; Attr_svg.y y
              ; Attr_svg.width width
              ; Attr_svg.height node_height
              ; Vdom.Attr.classes classes
              ; Vdom.Attr.on_click (fun _ ->
                  set_focus (Some { Node.tree_node = node; shape }))
              ; Vdom.Attr.on_double_click (fun _ -> set_zoom (Some node))
              ]
              [ Node_svg.rect
                  [ Vdom.Attr.class_ "flame-graph-node-color-box"
                  ; Vdom.Attr.style style
                  ; (* Since we set the width on the svg element above, we should be able to
                       let CSS take care of setting this rect's width to 100% of its container
                       and be done with it, but Chrome was sporadically making the rect very
                       small for no apparent reason. Height doesn't seem to have the same
                       problem. *)
                    Attr_svg.width width
                  ]
                  []
              ; Node_svg.text
                  [ Attr_svg.x 2.; Attr_svg.y (0.8 *. node_height) ]
                  [ Vdom.Node.text (Tree.Node.label ~shape node) ]
              ; Node_svg.title [] [ Vdom.Node.text (Tree.Node.details ~shape node) ]
              ]))
    in
    (view |> Option.to_list) @ children

  and render_level
        ~x
        ~y
        ~width
        ~shape
        ~size
        ~focus
        ~set_focus
        ~in_zoom
        ~zoom_nodes
        ~set_zoom
        ~even
        nodes
    =
    if Float.(size = 0.)
    then []
    else (
      let size_scale = width /. size in
      let x = ref x in
      let node_views =
        List.concat_map nodes ~f:(fun node ->
          let this_x = !x in
          let width = Tree.Node.size node *. size_scale in
          x := !x +. width;
          render_node
            ~x:this_x
            ~y
            ~width
            ~shape
            ~focus
            ~set_focus
            ~in_zoom
            ~zoom_nodes
            ~set_zoom
            ~even
            node)
      in
      (* If there's a single child, flatten the DOM out a little; if nothing else, it
         makes it easier to poke around using the Chrome inspector *)
      match nodes with
      | [] | [ _ ] -> node_views
      | _ -> [ Node_svg.g [] node_views ])
  ;;

  let tree_height ~shape (roots : Tree.Node.t list) =
    let rec height roots =
      let node_height node =
        let this_node_height = if Tree.Node.hidden ~shape node then 0 else 1 in
        this_node_height + height (Tree.Node.children ~shape node)
      in
      List.fold_left roots ~init:0 ~f:(fun h root -> Int.max h (node_height root))
    in
    height roots
  ;;

  let get_zoom_nodes zoom =
    let rec loop node acc =
      match Tree.Node.parent ~shape:Flames node with
      | None -> acc
      | Some parent -> loop parent (node :: acc)
    in
    loop zoom []
  ;;

  let render ~width ~focus ~zoom ~set_focus ~set_zoom =
    Random.init 0;
    let first_zoom_node, zoom_nodes =
      match get_zoom_nodes zoom with
      | [] -> assert false
      | first_zoom_node :: zoom_nodes -> first_zoom_node, zoom_nodes
    in
    let node_height_int = Float.round_up node_height |> Float.to_int in
    let flame_height_in_nodes =
      tree_height ~shape:Flames [ zoom ] + List.length zoom_nodes
    in
    let flame_height = node_height_int * flame_height_in_nodes in
    let icicle_roots = Tree.Node.children ~shape:Icicles zoom in
    let icicle_height = node_height_int * tree_height ~shape:Icicles icicle_roots in
    let height = flame_height + icicle_height in
    let flame_graph_bottom_node_y = Float.of_int flame_height -. node_height in
    let flame_node_views =
      let width = Float.of_int width in
      let size = Tree.Node.size first_zoom_node in
      render_level
        ~x:0.
        ~y:flame_graph_bottom_node_y
        ~width
        ~shape:Flames
        ~size
        ~focus
        ~set_focus
        ~in_zoom:true
        ~zoom_nodes
        ~set_zoom
        ~even:false
        [ first_zoom_node ]
    in
    let icicle_graph_top_node_y = Float.of_int flame_height in
    let icicle_node_views =
      let width = Float.of_int width in
      let zoom_nodes = [] in
      let size =
        List.fold_left icicle_roots ~init:0. ~f:(fun total child ->
          total +. Tree.Node.size child)
      in
      let icicle_roots_width =
        if flame_height_in_nodes = 0
        then
          (* No nodes above the top of the icicles, so let the icicles take up the whole
             width *)
          width
        else width *. (size /. Tree.Node.size zoom)
      in
      let x = (width -. icicle_roots_width) /. 2. in
      render_level
        ~x
        ~y:icicle_graph_top_node_y
        ~width:icicle_roots_width
        ~shape:Icicles
        ~size
        ~focus
        ~set_focus
        ~in_zoom:false
        ~zoom_nodes
        ~set_zoom
        ~even:false
        icicle_roots
    in
    let open Vdom in
    let flame_view = Node_svg.g [ Attr.class_ "flame-graph-flames" ] flame_node_views in
    let icicle_view =
      Node_svg.g [ Attr.class_ "flame-graph-icicles" ] icicle_node_views
    in
    Node.div
      [ Attr.class_ "flame-graph-container" ]
      [ Node_svg.svg
          [ Attr.class_ "flame-graph"
          ; Attr.style
              (Css_gen.concat [ Css_gen.width (`Px width); Css_gen.height (`Px height) ])
          ]
          [ flame_view; icicle_view ]
      ]
  ;;

  module Keyboard_navigation = struct
    module Rel_dir = struct
      type t =
        | Left
        | Right
        | Toward_roots
        | Toward_leaves

      let of_abs (abs : Direction.t) ~(shape : Shape.t) =
        match abs, shape with
        | Left, _ -> Left
        | Right, _ -> Right
        | Up, Flames | Down, Icicles -> Toward_leaves
        | Down, Flames | Up, Icicles -> Toward_roots
      ;;
    end

    let to_child ~node =
      let nodes = Node.children node in
      List.max_elt nodes ~compare:(fun (n1 : Node.t) (n2 : Node.t) ->
        Float.compare (Tree.Node.size n1.tree_node) (Tree.Node.size n2.tree_node))
    ;;

    let to_parent ~zoom ~node =
      let is_eligible_parent parent =
        match Node.parent parent with
        | None -> (* Don't focus root *) false
        | Some _ ->
          if Tree.Node.same parent.tree_node zoom
          then (
            match node.Node.shape with
            | Flames -> (* Zoom is in the flame part, so fine *) true
            | Icicles -> (* We're moving out of the icicle part *) false)
          else (* Normal case *) true
      in
      match Node.parent node with
      | Some parent when is_eligible_parent parent -> Some parent
      | _ ->
        (* Moving between shapes *)
        (match node.shape with
         | Flames ->
           let node = { Node.tree_node = zoom; shape = Icicles } in
           to_child ~node
         | Icicles ->
           let rec to_child_of_root tree_node =
             match Tree.Node.parent tree_node ~shape:Flames with
             | Some parent when Option.is_some (Tree.Node.parent parent ~shape:Icicles) ->
               to_child_of_root parent
             | _ -> tree_node
           in
           let tree_node = to_child_of_root zoom in
           Some { Node.tree_node; shape = Flames })
    ;;

    type left_or_right =
      | Left
      | Right

    let to_sibling ~which ~node =
      let%bind.Option parent = Node.parent node in
      let siblings = Node.children parent in
      let rec find_successor nodes =
        match nodes with
        | first_node :: next_node :: _
          when Tree.Node.same first_node.Node.tree_node node.tree_node -> Some next_node
        | _ :: nodes -> find_successor nodes
        | [] -> None
      in
      let siblings_oriented =
        match which with
        | Right -> siblings
        | Left -> List.rev siblings
      in
      find_successor siblings_oriented
    ;;

    let find_rel ~dir ~zoom ~node : Node.t option =
      match dir with
      | Rel_dir.Toward_roots -> to_parent ~zoom ~node
      | Toward_leaves -> to_child ~node
      | Left -> to_sibling ~which:Left ~node
      | Right -> to_sibling ~which:Right ~node
    ;;

    let find_abs ~dir ~zoom ~node =
      let%bind.Option node =
        match node with
        | Some node -> Some node
        | None ->
          (match dir with
           | Direction.Up -> Some { Node.shape = Flames; tree_node = zoom }
           | Down -> Some { Node.shape = Icicles; tree_node = zoom }
           | Left | Right -> None)
      in
      let dir = Rel_dir.of_abs dir ~shape:node.Node.shape in
      find_rel ~dir ~zoom ~node
    ;;

    let move ~(dir : Direction.t) ~zoom ~focus ~set_focus =
      let new_focus_node = find_abs ~dir ~zoom ~node:focus in
      match new_focus_node with
      | Some new_focus_node ->
        Vdom.Event.Many [ Vdom.Event.Prevent_default; set_focus (Some new_focus_node) ]
      | None -> Vdom.Event.Ignore
    ;;

    let handle_arrow_key ~zoom ~focus ~set_focus event =
      let dir : Direction.t =
        match Vdom_keyboard.Keyboard_event.key event with
        | ArrowUp | KeyK -> Up
        | ArrowDown | KeyJ -> Down
        | ArrowLeft | KeyH -> Left
        | ArrowRight | KeyL -> Right
        | _ -> assert false
      in
      move ~dir ~zoom ~focus ~set_focus
    ;;

    let arrow_key_command ~zoom ~focus ~set_focus =
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
      let description = "Move focus" in
      let group = None in
      let handler = handle_arrow_key ~zoom ~focus ~set_focus in
      { Keyboard_event_handler.Command.keys; description; group; handler }
    ;;

    let key_handler ~zoom ~focus ~set_focus =
      Vdom_keyboard.Keyboard_event_handler.of_command_list_exn
        [ arrow_key_command ~zoom ~focus ~set_focus ]
    ;;
  end

  type t =
    { view : Vdom.Node.t
    ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
    }

  let component ~tree ~width ~focus ~set_focus ~zoom ~set_zoom =
    let open Bonsai.Let_syntax in
    return
      (let%map tree = tree
       and width = width
       and focus = focus
       and set_focus = set_focus
       and zoom = zoom
       and set_zoom = set_zoom in
       (* Treat the focus as None if it's not in the tree or it's neither inside the zoom
          nor part of the zoom *)
       let focus =
         let%bind.Option focus = focus in
         let within_zoom =
           Tree.is_related
             tree
             ~shape:focus.Node.shape
             ~strictly:true
             ~ancestor:zoom
             ~descendant:focus.tree_node
         in
         let part_of_zoom () =
           Shape.equal focus.shape Flames
           && Tree.is_related
                tree
                ~shape:Flames
                ~strictly:false
                ~ancestor:focus.tree_node
                ~descendant:zoom
         in
         if within_zoom || part_of_zoom () then Some focus else None
       in
       let view = render ~width ~focus ~zoom ~set_focus ~set_zoom in
       let key_handler = Keyboard_navigation.key_handler ~zoom ~focus ~set_focus in
       { view; key_handler })
  ;;
end
