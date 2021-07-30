open! Core
open Bonsai_web
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr
include Flame_graph_view_intf


let node_height = 20.
let node_height_int = Float.round_up node_height |> Float.to_int

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

  let focus_color_odd =
    let r = 250 in
    let g = 250 in
    let b = 210 in
    `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ())
  ;;

  let focus_color_even =
    let r = 254 in
    let g = 254 in
    let b = 246 in
    `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ())
  ;;

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

  let unselected_classes_attr = Vdom.Attr.classes [ "flame-graph-node" ]

  let selected_classes_attr =
    Vdom.Attr.classes [ "flame-graph-node"; "flame-graph-node-selected" ]
  ;;

  let color_box_class_attr = Vdom.Attr.class_ "flame-graph-node-color-box"
  let height_attr = Attr_svg.height node_height
  let text_x_attr = Attr_svg.x 2.
  let text_y_attr = Attr_svg.y (0.8 *. node_height)

  let render_node
        ~graph
        ~x
        ~y
        ~width
        ~style
        ~selection
        ~select
        ~activate
        ~even
        ~selector
        node
    : Vdom.Node.t
    =
    let css = Style.get_node_css even style in
    let selected = Option.exists ~f:(Selector.same selector) selection in
    let classes_attr =
      if selected then selected_classes_attr else unselected_classes_attr
    in
    (* Put the node in an embedded <svg> element so that the text gets clipped to the
       rectangle *)
    Node_svg.svg
      ~attr:
        (Vdom.Attr.many
           [ Attr_svg.x x
           ; Attr_svg.y y
           ; Attr_svg.width width
           ; height_attr
           ; classes_attr
           ; Vdom.Attr.on_click (fun _ -> select selector)
           ; Vdom.Attr.on_double_click (fun _ -> activate selector)
           ])
      [ Node_svg.rect
          ~attr:
            (Vdom.Attr.many
               [ color_box_class_attr
               ; Vdom.Attr.style css
               ; (* Since we set the width on the svg element above, we should be able to
                    let CSS take care of setting this rect's width to 100% of its container
                    and be done with it, but Chrome was sporadically making the rect very
                    small for no apparent reason. Height doesn't seem to have the same
                    problem. *)
                 Attr_svg.width width
               ])
          []
      ; Node_svg.text
          ~attr:(Vdom.Attr.many [ text_x_attr; text_y_attr ])
          [ Vdom.Node.text (Graph.Node.label ~graph node) ]
      ; Node_svg.title [ Vdom.Node.text (Graph.Node.details ~graph node) ]
      ]
  ;;

  let rec render_sequence
            ~graph
            ~x
            ~y
            ~scale
            ~style
            ~bearing
            ~selection
            ~select
            ~activate
            ~even
            ~make_selector
            seq
    : Vdom.Node.t list
    =
    let first = Graph.Sequence.node seq in
    let first_view =
      let size = Graph.Node.size ~graph first in
      let width = size *. scale in
      let selector = make_selector seq in
      render_node
        ~graph
        ~x
        ~y
        ~width
        ~style
        ~selection
        ~select
        ~activate
        ~even
        ~selector
        first
    in
    let rest_view =
      match Graph.Sequence.next ~graph seq with
      | None -> []
      | Some rest ->
        let even = not even in
        let y = Bearing.succ_y ~bearing y in
        render_sequence
          ~graph
          ~x
          ~y
          ~scale
          ~style
          ~bearing
          ~selection
          ~select
          ~activate
          ~even
          ~make_selector
          rest
    in
    first_view :: rest_view
  ;;

  let rec render_tree
            ~graph
            ~x
            ~y
            ~scale
            ~style
            ~bearing
            ~selection
            ~select
            ~activate
            ~even
            ~make_selector
            tree
    : Vdom.Node.t list
    =
    let node = Graph.Tree.node tree in
    let width = Graph.Node.size ~graph node *. scale in
    let view =
      let selector = make_selector tree in
      render_node
        ~graph
        ~x
        ~y
        ~width
        ~style
        ~selection
        ~select
        ~activate
        ~even
        ~selector
        node
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
      render_level
        ~graph
        ~x
        ~y
        ~scale
        ~style
        ~bearing
        ~selection
        ~select
        ~activate
        ~even
        ~make_selector
        children
    in
    view :: children_view

  and render_level
        ~graph
        ~x
        ~y
        ~scale
        ~style
        ~bearing
        ~selection
        ~select
        ~activate
        ~even
        ~make_selector
        trees
    =
    let x = ref x in
    let tree_views =
      List.concat_map trees ~f:(fun tree ->
        let this_x = !x in
        let node = Graph.Tree.node tree in
        let width = Graph.Node.size ~graph node *. scale in
        x := !x +. width;
        render_tree
          ~graph
          ~x:this_x
          ~y
          ~scale
          ~style
          ~bearing
          ~selection
          ~select
          ~activate
          ~even
          ~make_selector
          tree)
    in
    (* If there's a single child, flatten the DOM out a little; if nothing else, it
       makes it easier to poke around using the Chrome inspector *)
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

  let render ~width ~selection ~select ~activate graph =
    Random.init 0;
    let flame_tree = Graph.flame_tree graph in
    let focus = Option.map ~f:(sequence_beginning ~graph) (Graph.focus graph) in
    let icicle_tree = Graph.icicle_tree graph in
    let size = Graph.size graph in
    let scale = Float.of_int width /. size in
    let flame_height = node_height_int * tree_height ~graph flame_tree in
    let focus_height =
      match focus with
      | None -> 0
      | Some seq -> node_height_int * sequence_height ~graph seq
    in
    let icicle_height = node_height_int * tree_height ~graph icicle_tree in
    let height = flame_height + focus_height + icicle_height in
    let flame_graph_bottom_node_y = Float.of_int flame_height -. node_height in
    let focus_bottom_node_y = Float.of_int (flame_height + focus_height) -. node_height in
    let icicle_graph_top_node_y = Float.of_int (flame_height + focus_height) in
    let flame_graph_views =
      render_level
        ~graph
        ~x:0.
        ~y:flame_graph_bottom_node_y
        ~scale
        ~style:Flames
        ~bearing:Upwards
        ~selection
        ~select
        ~activate
        ~even:false
        ~make_selector:Selector.flame
        flame_tree
    in
    let focus_views =
      match focus with
      | None -> []
      | Some focus ->
        render_sequence
          ~graph
          ~x:0.
          ~y:focus_bottom_node_y
          ~scale
          ~style:Focus
          ~bearing:Upwards
          ~selection
          ~select
          ~activate
          ~even:false
          ~make_selector:Selector.focus
          focus
    in
    let icicle_graph_views =
      render_level
        ~graph
        ~x:0.
        ~y:icicle_graph_top_node_y
        ~scale
        ~style:Icicles
        ~bearing:Downwards
        ~selection
        ~select
        ~activate
        ~even:false
        ~make_selector:Selector.icicle
        icicle_tree
    in
    let open Vdom in
    let flame_view =
      Node_svg.g ~attr:(Attr.class_ "flame-graph-flames") flame_graph_views
    in
    let focus_view = Node_svg.g ~attr:(Attr.class_ "flame-graph-focus") focus_views in
    let icicle_view =
      Node_svg.g ~attr:(Attr.class_ "flame-graph-icicles") icicle_graph_views
    in
    Node.div
      ~attr:(Attr.class_ "flame-graph-container")
      [ Node_svg.svg
          ~attr:
            (Attr.many
               [ Attr.class_ "flame-graph"
               ; Attr.style
                   (Css_gen.concat
                      [ Css_gen.width (`Px width); Css_gen.height (`Px height) ])
               ])
          [ flame_view; focus_view; icicle_view ]
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

    let arrow_key_command ~graph ~selection ~navigate_to =
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
      { Keyboard_event_handler.Command.keys; description; group; handler }
    ;;

    let key_handler ~graph ~selection ~navigate_to =
      Vdom_keyboard.Keyboard_event_handler.of_command_list_exn
        [ arrow_key_command ~graph ~selection ~navigate_to ]
    ;;
  end

  type t =
    { view : Vdom.Node.t
    ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
    }

  let component graph ~width ~selection ~select ~navigate_to ~activate =
    let open Bonsai.Let_syntax in
    return
      (let%map graph = graph
       and width = width
       and selection = selection
       and select = select
       and navigate_to = navigate_to
       and activate = activate in
       let view = render ~width ~selection ~select ~activate graph in
       let key_handler = Keyboard_navigation.key_handler ~graph ~selection ~navigate_to in
       { view; key_handler })
  ;;
end
