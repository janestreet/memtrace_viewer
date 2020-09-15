open! Core_kernel
open Bonsai_web
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr
include Flame_graph_view_intf

module Shape = struct
  type t =
    | Flames
    | Icicles
end

let node_height = 20.

module Make (Tree : Tree) = struct
  let color_for_node ~shape (_node : Tree.Node.t) =
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
            ~(shape : Shape.t)
            ~size_scale
            ~focus
            ~set_focus
            (node : Tree.Node.t)
    =
    let color = color_for_node ~shape node |> Css_gen.Color.to_string_css in
    let children =
      let child_nodes = Tree.Node.children node in
      let width_of_children =
        let size_of_children =
          List.fold_left child_nodes ~init:0. ~f:(fun total child ->
            total +. Tree.Node.size child)
        in
        size_of_children *. size_scale
      in
      let x = x +. ((width -. width_of_children) /. 2.) in
      let y =
        match shape with
        | Flames -> y -. node_height
        | Icicles -> y +. node_height
      in
      render_level ~x ~y ~shape ~size_scale ~focus ~set_focus child_nodes
    in
    let handle = Tree.Node.handle node in
    let focused =
      match focus with
      | Some focus -> Tree.Handle.equal handle focus
      | None -> false
    in
    let classes =
      List.filter_opt
        [ Some "flame-graph-node"; Option.some_if focused "flame-graph-node-focused" ]
    in
    Node_svg.g
      []
      [ (* Put the node in an embedded <svg> element so that the text gets clipped to the
           rectangle *)
        Node_svg.svg
          [ Attr_svg.x x
          ; Attr_svg.y y
          ; Attr_svg.width width
          ; Attr_svg.height node_height
          ; Vdom.Attr.classes classes
          ; Vdom.Attr.on_click (fun _ -> set_focus (Some handle))
          ]
          [ Node_svg.rect
              [ Vdom.Attr.class_ "flame-graph-node-color-box"
              ; Vdom.Attr.style (Css_gen.create ~field:"fill" ~value:color)
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
              [ Vdom.Node.text (Tree.Node.label node) ]
          ; Node_svg.title [] [ Vdom.Node.text (Tree.Node.details node) ]
          ]
      ; children
      ]

  and render_level ~x ~y ~shape ~size_scale ~focus ~set_focus nodes =
    let x = ref x in
    let node_views =
      List.map nodes ~f:(fun (node : Tree.Node.t) ->
        let this_x = !x in
        let width = Tree.Node.size node *. size_scale in
        x := !x +. width;
        render_node ~x:this_x ~y ~width ~shape ~size_scale ~focus ~set_focus node)
    in
    Node_svg.g [] node_views
  ;;

  let rec tree_height roots =
    let node_height node = 1 + tree_height (Tree.Node.children node) in
    List.fold_left roots ~init:0 ~f:(fun h root -> Int.max h (node_height root))
  ;;

  let render ~tree ~(shape : Shape.t) ~width ~focus ~zoom ~set_focus =
    Random.init 0;
    let zoomed_node =
      let%bind.Option zoom = zoom in
      Tree.find tree zoom
    in
    let roots =
      match zoomed_node with
      | Some zoomed_node -> Tree.Node.children zoomed_node
      | None -> Tree.roots tree
    in
    let height = Float.to_int (Float.round_up node_height) * tree_height roots in
    let total_size =
      List.fold_left roots ~init:0. ~f:(fun total (root : Tree.Node.t) ->
        total +. Tree.Node.size root)
    in
    let size_scale = Float.of_int width /. total_size in
    let y =
      match shape with
      | Flames -> Float.of_int height -. node_height
      | Icicles -> 0.
    in
    let roots_view = render_level ~x:0. ~y ~shape ~size_scale ~focus ~set_focus roots in
    let shape_class =
      match shape with
      | Flames -> "flame-shaped"
      | Icicles -> "icicle-shaped"
    in
    Vdom.Node.div
      [ Vdom.Attr.class_ "flame-graph-container" ]
      [ Node_svg.svg
          [ Vdom.Attr.classes [ "flame-graph"; shape_class ]
          ; Vdom.Attr.style
              (Css_gen.concat [ Css_gen.width (`Px width); Css_gen.height (`Px height) ])
          ]
          [ roots_view ]
      ]
  ;;

  module Keyboard_navigation = struct
    module Abs_dir = struct
      type t =
        | Left
        | Right
        | Up
        | Down
    end

    module Rel_dir = struct
      type t =
        | Left
        | Right
        | Toward_roots
        | Toward_leaves

      let of_abs (abs : Abs_dir.t) ~(shape : Shape.t) =
        match abs, shape with
        | Left, _ -> Left
        | Right, _ -> Right
        | Up, Icicles | Down, Flames -> Toward_roots
        | Down, Icicles | Up, Flames -> Toward_leaves
      ;;
    end

    let find_rel ~tree ~dir ~handle =
      let node = Option.bind ~f:(Tree.find tree) handle in
      match dir, node with
      | Rel_dir.Toward_roots, Some node -> Tree.find_parent tree node
      | Toward_leaves, _ ->
        let nodes =
          match node with
          | None -> Tree.roots tree
          | Some node -> Tree.Node.children node
        in
        List.max_elt nodes ~compare:(fun n1 n2 ->
          Float.compare (Tree.Node.size n1) (Tree.Node.size n2))
      | Left, Some node | Right, Some node ->
        let siblings =
          match Tree.find_parent tree node with
          | Some parent -> Tree.Node.children parent
          | None -> Tree.roots tree
        in
        let rec find_successor nodes =
          match nodes with
          | first_node :: next_node :: _
            when Tree.Handle.equal (Tree.Node.handle first_node) (Tree.Node.handle node)
            -> Some next_node
          | _ :: nodes -> find_successor nodes
          | [] -> None
        in
        let siblings_oriented =
          match dir with
          | Right -> siblings
          | Left -> List.rev siblings
          | _ -> assert false
        in
        find_successor siblings_oriented
      | _ -> None
    ;;

    let find_abs ~tree ~dir ~shape ~handle =
      let dir = Rel_dir.of_abs ~shape dir in
      find_rel ~tree ~dir ~handle
    ;;

    let move ~tree ~(dir : Abs_dir.t) ~shape ~focus ~set_focus =
      let new_focus_node = find_abs ~tree ~dir ~shape ~handle:focus in
      match new_focus_node with
      | Some new_focus_node ->
        Vdom.Event.Many
          [ Vdom.Event.Prevent_default
          ; set_focus (Some (Tree.Node.handle new_focus_node))
          ]
      | None -> Vdom.Event.Ignore
    ;;

    let handle_arrow_key ~tree ~shape ~focus ~set_focus event =
      let dir : Abs_dir.t =
        match Vdom_keyboard.Keyboard_event.key event with
        | ArrowUp | KeyK -> Up
        | ArrowDown | KeyJ -> Down
        | ArrowLeft | KeyH -> Left
        | ArrowRight | KeyL -> Right
        | _ -> assert false
      in
      move ~dir ~tree ~shape ~focus ~set_focus
    ;;

    let arrow_key_command ~tree ~shape ~focus ~set_focus =
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
      let handler = handle_arrow_key ~tree ~shape ~focus ~set_focus in
      { Keyboard_event_handler.Command.keys; description; group; handler }
    ;;

    let key_handler ~tree ~shape ~focus ~set_focus =
      Vdom_keyboard.Keyboard_event_handler.of_command_list_exn
        [ arrow_key_command ~tree ~shape ~focus ~set_focus ]
    ;;
  end

  type t =
    { view : Vdom.Node.t
    ; focus : Tree.Handle.t option
    ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
    }
  [@@deriving fields]

  module Component = struct
    let name = "Flame graph"

    module Input = struct
      type t =
        { tree : Tree.t
        ; shape : Shape.t
        ; width : int
        ; zoom : Tree.Handle.t option
        }
    end

    module Result = struct
      type nonrec t = t
    end

    module Action = struct
      type t = Set_focus of Tree.Handle.t option [@@deriving sexp_of]
    end

    module Model = struct
      type t = { focus : Tree.Handle.t option } [@@deriving sexp, equal]

      let default = { focus = None }
    end

    let apply_action ~inject:_ ~schedule_event:_ _ _ = function
      | Action.Set_focus h -> { Model.focus = h }
    ;;

    let compute ~inject ({ tree; shape; width; zoom } : Input.t) { Model.focus } =
      (* Treat the focus as None if it's not in the tree or not inside the zoom *)
      let focus =
        let%bind.Option handle = focus in
        let%bind.Option focus_node = Tree.find tree handle in
        let within_zoom =
          let zoom_node = Option.bind ~f:(Tree.find tree) zoom in
          match zoom_node with
          | None -> true
          | Some zoom_node ->
            Tree.is_related tree ~strictly:true ~ancestor:zoom_node ~descendant:focus_node
        in
        if within_zoom then focus else None
      in
      let set_focus handle = inject (Action.Set_focus handle) in
      let view = render ~tree ~shape ~width ~focus ~zoom ~set_focus in
      let key_handler = Keyboard_navigation.key_handler ~tree ~shape ~focus ~set_focus in
      ({ view; focus; key_handler } : Result.t)
    ;;
  end

  let component ~tree ~shape ~width ~zoom =
    let open Bonsai.Let_syntax in
    let input =
      let%map tree = tree
      and shape = shape
      and width = width
      and zoom = zoom in
      { Component.Input.tree; shape; width; zoom }
    in
    Bonsai.of_module1 (module Component) input ~default_model:Component.Model.default
  ;;
end
