open! Core_kernel
open! Bonsai_web
open Memtrace_viewer_common
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
    ; range : Range.Time_ns_span.Or_empty.t
    }

  let create ?css_class range = { css_class; range }
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

module Viewport : sig
  type t [@@deriving equal]

  val create
    :  width:float
    -> height:float
    -> pos:float * float
    -> max_x:Time_ns.Span.t
    -> max_y:float
    -> t

  val x_coord_of : t -> Time_ns.Span.t -> float
  val y_coord_of : t -> float -> float
  val coords_of : t -> Time_ns.Span.t * float -> float * float
  val left : t -> float
  val right : t -> float
  val top : t -> float
  val bottom : t -> float
  val width : t -> float
  val height : t -> float
end = struct
  type t =
    { origin_x : float
    ; origin_y : float
    ; scale_x : float
    ; scale_y : float
    ; top : float
    ; right : float
    ; width : float
    ; height : float
    }
  [@@deriving equal]

  let create ~width ~height ~pos:(pos_x, pos_y) ~max_x ~max_y =
    let right = pos_x +. width in
    let top = pos_y in
    (* We need to flip the y-direction. To do so, we set the origin at the
       lower-*left* corner and make [scale_y] negative. *)
    let origin_x = pos_x in
    let origin_y = pos_y +. height in
    let scale_x =
      if Time_ns.Span.(max_x = zero) then 0. else width /. (max_x |> Time_ns.Span.to_sec)
    in
    let scale_y = if Float.(max_y = 0.) then 0. else ~-.height /. max_y in
    { origin_x; origin_y; scale_x; scale_y; top; right; width; height }
  ;;

  let x_coord_of t x = t.origin_x +. ((x |> Time_ns.Span.to_sec) *. t.scale_x)
  let y_coord_of t y = t.origin_y +. (y *. t.scale_y)
  let coords_of t (x, y) = x_coord_of t x, y_coord_of t y
  let left t = t.origin_x
  let right t = t.right
  let top t = t.top
  let bottom t = t.origin_y
  let width t = t.width
  let height t = t.height
end

let _ = Viewport.y_coord_of (* seems silly to leave it out *)

let render_graph_line ~viewport (series : Series.t) =
  let points = List.map ~f:(Viewport.coords_of viewport) series.points in
  let classes = List.filter_opt [ Some "graph-line"; series.css_class ] in
  Node_svg.polyline [ Attr.classes classes; Attr_svg.points points ] []
;;

(* Important that this have as few inputs as possible, since this is the expensive part *)
let graph_lines ~viewport series =
  let open Bonsai.Let_syntax in
  return
    (let%map viewport = viewport
     and series = series in
     List.map ~f:(render_graph_line ~viewport) series)
;;

let component ~series ~regions ~width ~height ~start_time ~time_view ~set_time_view
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let open Bonsai.Value.Applicative_infix in
  let width = width >>| Float.of_int in
  let height = height >>| Float.of_int in
  let max_xy =
    let%map series = series in
    let max_x, max_y =
      List.fold_left
        series
        ~init:(Time_ns.Span.zero, 0.)
        ~f:(fun (max_x, max_y) (series : Series.t) ->
          Time_ns.Span.max max_x series.max_x, Float.max max_y series.max_y)
    in
    (* Add a bit of headroom so that points at the top of the range are still visible *)
    let max_y = max_y *. 1.05 in
    max_x, max_y
  in
  let viewport =
    let%map max_x, max_y = max_xy
    and width = width
    and height = height
    and time_view = time_view in
    let data_pos_x = width *. 0.05 in
    let data_pos_y = 0. in
    let data_width = width *. 0.9 in
    let data_height =
      match time_view with
      | Time_view.Elapsed_seconds -> 0.9 *. height
      | Wall_time -> 0.75 *. height
    in
    Viewport.create
      ~width:data_width
      ~height:data_height
      ~pos:(data_pos_x, data_pos_y)
      ~max_x
      ~max_y
  in
  let%sub graph_lines = graph_lines ~viewport series in
  return
    (let%map regions = regions
     and width = width
     and height = height
     and start_time = start_time
     and time_view = time_view
     and set_time_view = set_time_view
     and max_x, _ = max_xy
     and viewport = viewport
     and graph_lines = graph_lines in
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
         Nice.Time_ns.loose_labels
           ~max_count:11
           ~relative_to:start_day
           start_time
           end_time
         |> List.map ~f:(fun t -> Time_ns.diff t start_time)
     in
     let tick_marks : Node.t list =
       List.map ticks ~f:(fun x ->
         let x = Viewport.x_coord_of viewport x in
         Node_svg.line
           [ Attr.class_ "graph-tick-mark"
           ; Attr_svg.x1 x
           ; Attr_svg.y1 (Viewport.bottom viewport -. (Viewport.height viewport /. 20.))
           ; Attr_svg.x2 x
           ; Attr_svg.y2 (Viewport.bottom viewport)
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
             | Elapsed_seconds -> Viewport.x_coord_of viewport x
             | Wall_time -> Viewport.x_coord_of viewport x -. 5.
           in
           let label_y = Viewport.bottom viewport +. 15. in
           let label =
             match time_view with
             | Elapsed_seconds -> sprintf "%g" (x |> Time_ns.Span.to_sec)
             | Wall_time ->
               let wall_time = Time_ns.add start_time x in
               let _, time_of_day =
                 Time_ns.to_date_ofday ~zone:Time.Zone.utc wall_time
               in
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
               [ Attr_svg.transform
                   [ Rotate { a = `Deg 20.; x = label_x; y = label_y } ]
               ]
           in
           Node_svg.text
             (List.concat
                [ [ Attr.classes classes; Attr_svg.x label_x; Attr_svg.y label_y ]
                ; transform_attrs
                ])
             [ Vdom.Node.text label ])
     in
     let region_box (region : Region.t) =
       match region.range with
       | Empty -> Util.placeholder_svg
       | Non_empty range when Range.Time_ns_span.is_all range -> Util.placeholder_svg
       | Non_empty range ->
         let bound_line bound =
           match bound with
           | Range.Bound.No_bound -> Util.placeholder_svg
           | Open x | Closed x ->
             let x1 = Viewport.x_coord_of viewport x in
             let y1 = Viewport.top viewport in
             let x2 = x1 in
             let y2 = Viewport.bottom viewport in
             let open_or_closed_class =
               match bound with
               | Open _ -> "graph-region-bound-open"
               | Closed _ -> "graph-region-bound-closed"
               | No_bound -> assert false
             in
             Node_svg.line
               [ Vdom.Attr.classes [ "graph-region-bound"; open_or_closed_class ]
               ; Attr_svg.x1 x1
               ; Attr_svg.y1 y1
               ; Attr_svg.x2 x2
               ; Attr_svg.y2 y2
               ]
               []
         in
         let lower_bound_line = bound_line range.lower_bound in
         let upper_bound_line = bound_line range.upper_bound in
         let interior =
           let lower_bound_x =
             match range.lower_bound with
             | No_bound -> Viewport.left viewport
             | Open lower_bound | Closed lower_bound ->
               Viewport.x_coord_of viewport lower_bound
           in
           let upper_bound_x =
             match range.upper_bound with
             | No_bound -> Viewport.right viewport
             | Open upper_bound | Closed upper_bound ->
               Viewport.x_coord_of viewport upper_bound
           in
           Node_svg.rect
             [ Vdom.Attr.class_ "graph-region-interior"
             ; Attr_svg.x lower_bound_x
             ; Attr_svg.y (Viewport.top viewport)
             ; Attr_svg.width (upper_bound_x -. lower_bound_x)
             ; Attr_svg.height (Viewport.height viewport)
             ]
             []
         in
         let classes = List.filter_opt [ Some "graph-region"; region.css_class ] in
         Node_svg.g
           [ Attr.classes classes ]
           [ interior; lower_bound_line; upper_bound_line ]
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
               ; Attr_svg.width (Viewport.width viewport)
               ; Attr_svg.height (Viewport.height viewport)
               ]
               [ Node_svg.rect
                   [ Attr.class_ "graph-border"
                   ; Attr_svg.x (Viewport.left viewport)
                   ; Attr_svg.y (Viewport.top viewport)
                   ; Attr_svg.width (Viewport.width viewport)
                   ; Attr_svg.height (Viewport.height viewport)
                   ]
                   []
               ; Node_svg.g [] graph_lines
               ; Node_svg.g [] tick_marks
               ]
           ; Node_svg.g [] tick_labels
           ; Node_svg.g [] region_boxes
           ]
       ; Node.div [ Attr.class_ "graph-x-axis-label" ] [ time_view_control ]
       ])
;;
