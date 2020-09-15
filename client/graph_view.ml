open! Core_kernel
open! Bonsai_web
module Attr = Vdom.Attr
module Node = Vdom.Node
module Attr_svg = Virtual_dom_svg.Attr
module Node_svg = Virtual_dom_svg.Node

module Series = struct
  type t =
    { css_class : string option
    ; points : (Time_ns.Span.t * float) list
    ; max_x : Time_ns.Span.t
    ; max_y : float
    }

  let create ?css_class ~max_x ~max_y points = { css_class; points; max_x; max_y }
end

module Region = struct
  type t =
    { css_class : string option
    ; lower_bound : Time_ns.Span.t option
    ; upper_bound : Time_ns.Span.t option
    }

  let create ?css_class lower_bound upper_bound = { css_class; lower_bound; upper_bound }
end

module Time_view = struct
  type t =
    | Elapsed_seconds
    | Wall_time
  [@@deriving equal, sexp]

  let to_string = function
    | Elapsed_seconds -> "Elapsed time (s)"
    | Wall_time -> "Wall time"
  ;;
end

let graph_view ~series ~regions ~width ~height ~start_time ~time_view ~set_time_view =
  let width = Float.of_int width in
  let height = Float.of_int height in
  let data_offset_x = width *. 0.05 in
  let data_offset_y = 0. in
  let data_width = width *. 0.9 in
  let data_height =
    match time_view with
    | Time_view.Elapsed_seconds -> 0.9 *. height
    | Wall_time -> 0.75 *. height
  in
  let max_x, max_y =
    List.fold_left
      series
      ~init:(Time_ns.Span.zero, 0.)
      ~f:(fun (max_x, max_y) (series : Series.t) ->
        Time_ns.Span.max max_x series.max_x, Float.max max_y series.max_y)
  in
  (* Add a bit of headroom so that points at the top of the range are still visible *)
  let max_y = max_y *. 1.05 in
  let x_scaling_factor =
    if Time_ns.Span.(max_x = zero)
    then 0.
    else data_width /. (max_x |> Time_ns.Span.to_sec)
  in
  let y_scaling_factor = if Float.(max_y = 0.) then 0. else data_height /. max_y in
  let x_coord_of x = data_offset_x +. ((x |> Time_ns.Span.to_sec) *. x_scaling_factor) in
  let y_coord_of y =
    (* Y coordinates are flipped in SVG coordinates *)
    data_offset_y +. ((max_y -. y) *. y_scaling_factor)
  in
  let coords_of_point (x, y) = x_coord_of x, y_coord_of y in
  let graph_lines =
    List.map series ~f:(fun (series : Series.t) ->
      let points = List.map ~f:coords_of_point series.points in
      let classes = List.filter_opt [ Some "graph-line"; series.css_class ] in
      Node_svg.polyline [ Attr.classes classes; Attr_svg.points points ] [])
  in
  let ticks =
    match time_view with
    | Elapsed_seconds ->
      (* Don't use Nice.Time_ns.Span yet because the ticks are (for now) just labeled as a
         number of seconds, and a nice number of minutes makes for a weird number of
         seconds *)
      Nice.loose_labels ~max_count:11 0. (max_x |> Time_ns.Span.to_sec)
      |> List.map ~f:(fun t -> t |> Time_ns.Span.of_sec)
    | Wall_time ->
      let end_time = Time_ns.add start_time max_x in
      let start_day = Nice.Time_ns.start_of_day_utc start_time in
      Nice.Time_ns.loose_labels ~max_count:11 ~relative_to:start_day start_time end_time
      |> List.map ~f:(fun t -> Time_ns.diff t start_time)
  in
  let tick_marks : Node.t list =
    List.map ticks ~f:(fun x ->
      let x = x_coord_of x in
      Node_svg.line
        [ Attr.class_ "graph-tick-mark"
        ; Attr_svg.x1 x
        ; Attr_svg.y1 (data_offset_y +. data_height -. (data_height /. 20.))
        ; Attr_svg.x2 x
        ; Attr_svg.y2 (data_offset_y +. data_height)
        ]
        [])
  in
  let tick_labels : Node.t list =
    if Time_ns.Span.(max_x = zero)
    then []
    else
      List.map ticks ~f:(fun x ->
        let label_x =
          match time_view with
          | Elapsed_seconds -> x_coord_of x
          | Wall_time -> x_coord_of x -. 5.
        in
        let label_y = data_offset_y +. data_height +. 15. in
        let label =
          match time_view with
          | Elapsed_seconds -> sprintf "%g" (x |> Time_ns.Span.to_sec)
          | Wall_time ->
            let wall_time = Time_ns.add start_time x in
            let _, time_of_day = Time_ns.to_date_ofday ~zone:Time.Zone.utc wall_time in
            Time_ns.Ofday.to_string_trimmed time_of_day
        in
        let classes =
          match time_view with
          | Elapsed_seconds -> [ "graph-label" ]
          | Wall_time -> [ "graph-label"; "graph-label-long" ]
        in
        let transform_attrs =
          match time_view with
          | Elapsed_seconds -> []
          | Wall_time ->
            [ Attr_svg.transform [ Rotate { a = `Deg 20.; x = label_x; y = label_y } ] ]
        in
        Node_svg.text
          (List.concat
             [ [ Attr.classes classes; Attr_svg.x label_x; Attr_svg.y label_y ]
             ; transform_attrs
             ])
          [ Vdom.Node.text label ])
  in
  let region_box (region : Region.t) =
    let bound_line x =
      let x1 = x_coord_of x in
      let y1 = data_offset_y in
      let x2 = x1 in
      let y2 = data_offset_y +. data_height in
      Node_svg.line
        [ Vdom.Attr.class_ "graph-region-bound"
        ; Attr_svg.x1 x1
        ; Attr_svg.y1 y1
        ; Attr_svg.x2 x2
        ; Attr_svg.y2 y2
        ]
        []
    in
    let lower_bound_line = Option.map ~f:bound_line region.lower_bound in
    let upper_bound_line = Option.map ~f:bound_line region.upper_bound in
    let interior =
      let lower_bound_x =
        match region.lower_bound with
        | None -> data_offset_x
        | Some lower_bound -> x_coord_of lower_bound
      in
      let upper_bound_x =
        match region.upper_bound with
        | None -> data_offset_x +. data_width
        | Some upper_bound -> x_coord_of upper_bound
      in
      Node_svg.rect
        [ Vdom.Attr.class_ "graph-region-interior"
        ; Attr_svg.x lower_bound_x
        ; Attr_svg.y data_offset_y
        ; Attr_svg.width (upper_bound_x -. lower_bound_x)
        ; Attr_svg.height data_height
        ]
        []
    in
    let classes = List.filter_opt [ Some "graph-region"; region.css_class ] in
    let nodes = List.filter_opt [ Some interior; lower_bound_line; upper_bound_line ] in
    Node_svg.g [ Attr.classes classes ] nodes
  in
  let region_boxes = List.map ~f:region_box regions in
  let time_view_control =
    Vdom_input_widgets.Dropdown.of_values
      (module Time_view)
      [ Time_view.Elapsed_seconds; Wall_time ]
      ~selected:time_view
      ~on_change:set_time_view
  in
  Node.div
    []
    [ Node_svg.svg
        [ Attr.id "filter-graph"
        ; Attr.class_ "graph"
        ; Attr_svg.width width
        ; Attr_svg.height height
        ; Attr_svg.viewbox ~min_x:0. ~min_y:0. ~width ~height
        ; Attr_svg.preserve_aspect_ratio ~align:None ()
        ]
        [ Node_svg.g
            [ Attr.class_ "graph-data"
            ; Attr_svg.x 0.
            ; Attr_svg.y 0.
            ; Attr_svg.width data_width
            ; Attr_svg.height data_height
            ]
            [ Node_svg.rect
                [ Attr.class_ "graph-border"
                ; Attr_svg.x data_offset_x
                ; Attr_svg.y data_offset_y
                ; Attr_svg.width data_width
                ; Attr_svg.height data_height
                ]
                []
            ; Node_svg.g [] graph_lines
            ; Node_svg.g [] tick_marks
            ]
        ; Node_svg.g [] tick_labels
        ; Node_svg.g [] region_boxes
        ]
    ; Node.div [ Attr.class_ "graph-x-axis-label" ] [ time_view_control ]
    ]
;;

let component ~series ~regions ~width ~height ~start_time ~time_view ~set_time_view
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  return
    (let%map series = series
     and regions = regions
     and width = width
     and height = height
     and start_time = start_time
     and time_view = time_view
     and set_time_view = set_time_view in
     graph_view ~series ~regions ~width ~height ~start_time ~time_view ~set_time_view)
;;
