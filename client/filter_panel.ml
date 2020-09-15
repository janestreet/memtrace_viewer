open! Core_kernel
open! Bonsai_web
module Attr = Vdom.Attr
module Node = Vdom.Node
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr
open Memtrace_viewer_common

module Ranges = struct
  type t =
    { allocated : Range_input.t
    ; live : Range_input.t
    }
end

let ranges ~(graph : Data.Graph.t Bonsai.Value.t) ~start_time ~time_view
  : Ranges.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let initial_value = Time_range.all in
  let max_x = Data.Graph.max_x <$> graph in
  let range = Range_input.component ~initial_value ~max:max_x ~start_time ~time_view in
  let%sub allocated = range in
  let%sub live = range in
  return
    (let%map allocated = allocated
     and live = live in
     { Ranges.allocated; live })
;;

module Direction_button = struct
  type t = Filter.direction =
    | Explore_downwards_from_allocations
    | Explore_upwards_from_main
  [@@deriving sexp, equal, enumerate]

  let label = function
    | Explore_downwards_from_allocations -> "Downwards from allocations"
    | Explore_upwards_from_main -> "Upwards from “main”"
  ;;

  let title = function
    | Explore_downwards_from_allocations ->
      Some
        "Group allocations starting from allocation sites. Good for finding localized \
         problems."
    | Explore_upwards_from_main ->
      Some
        "Group allocations starting from the top-level “main” function. Good for \
         seeing an overview of which parts of a program are allocating the most."
  ;;
end

let direction_list : Filter.direction Radio_list.t Bonsai.Computation.t =
  Radio_list.component
    (module Direction_button)
    ~name:"direction"
    ~initial_value:Explore_downwards_from_allocations
;;

module Heap_filter = struct
  type t =
    { include_minor_heap : Checkbox.t
    ; include_major_heap : Checkbox.t
    }

  let view { include_minor_heap; include_major_heap } =
    Node.ul
      [ Attr.class_ "checkbox-list" ]
      [ Node.li [] [ Checkbox.view include_minor_heap ]
      ; Node.li [] [ Checkbox.view include_major_heap ]
      ]
  ;;
end

let heap_filter : Heap_filter.t Bonsai.Computation.t =
  let open Bonsai.Let_syntax in
  let%sub include_minor_heap =
    Checkbox.component ~label:"Include minor heap" ~initial_value:true
  in
  let%sub include_major_heap =
    Checkbox.component ~label:"Include major heap" ~initial_value:true
  in
  return
    (let%map include_minor_heap = include_minor_heap
     and include_major_heap = include_major_heap in
     { Heap_filter.include_minor_heap; include_major_heap })
;;

let graph_view
      ~graph
      ~filtered_graph
      ~allocated_range
      ~live_range
      ~start_time
      ~time_view
      ~set_time_view
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let width = 500 in
  let height = 200 in
  let series_and_regions =
    let%map graph = graph
    and filtered_graph = filtered_graph
    and allocated_range = allocated_range
    and live_range = live_range in
    let max_x =
      match filtered_graph with
      | None -> Data.Graph.max_x graph
      | Some filtered_graph ->
        Time_ns.Span.max (Data.Graph.max_x graph) (Data.Graph.max_x filtered_graph)
    in
    let series_of_data_graph css_class (graph : Data.Graph.t) =
      let max_y = Data.Graph.max_y graph |> Byte_units.bytes_float in
      let points_rev =
        List.rev_map (Data.Graph.points graph) ~f:(fun (x, y) ->
          x, y |> Byte_units.bytes_float)
      in
      (* Make sure the graph has points at x=0 and x=max_x so that it ranges through the
         whole timeline. We can assume that the total allocations start at 0 and don't
         change after the last reported point. *)
      let last_x, last_y =
        match points_rev with
        | last_point :: _ -> last_point
        | [] -> Time_ns.Span.zero, 0.
      in
      let points_rev =
        if Time_ns.Span.(last_x < max_x)
        then (max_x, last_y) :: points_rev
        else points_rev
      in
      let points = List.rev points_rev in
      let points =
        match points with
        | (x, _) :: _ when Time_ns.Span.(x = zero) -> points
        | _ -> (Time_ns.Span.zero, 0.) :: points
      in
      Graph_view.Series.create ~css_class ~max_x ~max_y points
    in
    let full_series = series_of_data_graph "full-graph-line" graph in
    let filtered_series =
      Option.map ~f:(series_of_data_graph "filtered-graph-line") filtered_graph
    in
    let series = List.filter_opt [ filtered_series; Some full_series ] in
    let region_of_range css_class range : Graph_view.Region.t option =
      if Time_range.covers range ~lower:Time_ns.Span.zero ~upper:(Data.Graph.max_x graph)
      then None
      else Some (Graph_view.Region.create ~css_class range.lower_bound range.upper_bound)
    in
    let allocated_region = region_of_range "graph-allocated-range" allocated_range in
    let live_region = region_of_range "graph-live-range" live_range in
    let regions = List.filter_opt [ allocated_region; live_region ] in
    series, regions
  in
  let series = fst <$> series_and_regions in
  let regions = snd <$> series_and_regions in
  let width = Bonsai.Value.return width in
  let height = Bonsai.Value.return height in
  Graph_view.component
    ~series
    ~regions
    ~width
    ~height
    ~start_time
    ~time_view
    ~set_time_view
;;

let button
      ~server_state
      ~inject_outgoing
      ~ranges:{ Ranges.allocated; live }
      ~direction
      ~heap_filter:{ Heap_filter.include_minor_heap; include_major_heap }
  =
  let on_submit =
    let outgoing_action : Memtrace_viewer_common.Action.t =
      let ranges : Filter.Ranges.t =
        { allocated_range = Range_input.range allocated
        ; live_range = Range_input.range live
        }
      in
      let direction : Filter.direction = Radio_list.value direction in
      let include_minor_heap = Checkbox.value include_minor_heap in
      let include_major_heap = Checkbox.value include_major_heap in
      Set_filter { ranges; direction; include_minor_heap; include_major_heap }
    in
    Vdom.Event.Many
      [ Vdom.Event.Prevent_default (* don't do a real HTML submit! *)
      ; Range_input.reset_changing allocated
      ; Range_input.reset_changing live
      ; Radio_list.reset_changing direction
      ; Checkbox.reset_changing include_minor_heap
      ; Checkbox.reset_changing include_major_heap
      ; inject_outgoing outgoing_action
      ]
  in
  let server_is_up = Server_state.Status.is_up Server_state.(server_state.status) in
  let something_is_changing =
    Radio_list.changing direction
    || Range_input.changing allocated
    || Range_input.changing live
    || Checkbox.changing include_minor_heap
    || Checkbox.changing include_major_heap
  in
  let heap_filters_not_both_false =
    Checkbox.value include_minor_heap || Checkbox.value include_major_heap
  in
  let enabled = server_is_up && something_is_changing && heap_filters_not_both_false in
  let view =
    Node.input
      (* Don't actually put an onclick handler on the button; just return the handler to
         be used as the form's onsubmit instead, thus getting Enter key behavior for free
      *)
      ([ Attr.type_ "submit"
       ; Attr.value "Apply"
       ; Attr.title "Set current filter and redraw views"
       ]
       @ if not enabled then [ Attr.disabled ] else [])
      []
  in
  view, on_submit
;;

let button ~ranges ~direction ~heap_filter ~server_state ~inject_outgoing
  : (Vdom.Node.t * Vdom.Event.t) Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  return
    (let%map server_state = server_state
     and inject_outgoing = inject_outgoing
     and ranges = ranges
     and direction = direction
     and heap_filter = heap_filter in
     button ~server_state ~inject_outgoing ~ranges ~direction ~heap_filter)
;;

let panel
      ~server_state
      ~time_view
      ~filtered_allocations
      ~allocated_nodes
      ~live_nodes
      ~range_size
      ~graph_node
      ~direction_list_node
      ~heap_filter_node
      ~button_node
      ~on_submit
  =
  let allocations_line =
    match filtered_allocations with
    | None -> Node.none
    | Some bytes ->
      Node.p
        [ Attr.class_ "total-allocations" ]
        [ Node.textf "Filtered allocations: %s" (bytes |> Byte_units.Short.to_string) ]
  in
  let range_rows ~header ~prefix ~inputs:(lower_input, upper_input) =
    let swatch_class = String.concat [ prefix; "-swatch" ] in
    let swatch =
      Node_svg.svg
        [ Attr.classes [ "swatch"; swatch_class ] ]
        [ Node_svg.rect [ Attr.class_ "swatch-bg" ] []
        ; Node_svg.rect [ Attr.class_ "swatch-interior" ] []
        ; Node_svg.rect [ Attr.class_ "swatch-border" ] []
        ]
    in
    let header_cells = [ Node.td [] [ swatch ]; Node.td [] [ Node.text header ] ] in
    let sec_node =
      match time_view with
      | Graph_view.Time_view.Elapsed_seconds -> Node.td [] [ Node.text "s" ]
      | Wall_time -> Node.none
    in
    match range_size with
    | Range_input.Size.Small ->
      [ Node.tr
          []
          (header_cells
           @ [ Node.td [] [ lower_input ]
             ; sec_node
             ; Node.td [] [ Node.text "and" ]
             ; Node.td [] [ upper_input ]
             ; sec_node
             ])
      ]
    | Large ->
      [ Node.tr [] (header_cells @ [ Node.td [] [ lower_input ]; sec_node ])
      ; Node.tr
          []
          [ Node.td [] []
          ; Node.td [] [ Node.text "and" ]
          ; Node.td [] [ upper_input ]
          ; sec_node
          ]
      ]
  in
  let connection_lost_message =
    match server_state with
    | { Server_state.status = Down } -> Node.text " Server connection lost"
    | _ -> Node.none
  in
  let form =
    Node.create
      "form"
      [ Attr.id "filter-form"
      ; Attr.on "submit" (fun _ -> on_submit)
      ; (* Browser-level validation isn't our friend - it rejects numeric inputs if the
           user inputs too many digits (as decided by the step) *)
        Attr.create "novalidate" ""
      ; (* This effectively disables the table or flame graph's keyboard event handler
           whenever the focus is anywhere in the form, so that Enter, arrow keys, etc.
           all work correcly *)
        Attr.on_keydown (fun _ -> Vdom.Event.Stop_propagation)
      ]
      [ Node.p [] [ Node.text "Only show allocations:" ]
      ; Node.table
          [ Attr.class_ "range-table" ]
          (List.concat
             [ range_rows
                 ~header:"Live somewhere between"
                 ~prefix:"live"
                 ~inputs:live_nodes
             ; range_rows
                 ~header:"Occurring between"
                 ~prefix:"allocated"
                 ~inputs:allocated_nodes
             ])
      ; heap_filter_node
      ; Node.p [] [ Node.text "Explore:" ]
      ; direction_list_node
      ; Node.div [] [ button_node; connection_lost_message ]
      ]
  in
  let header_text = Vdom.Node.text "Filter" in
  let main_body =
    Vdom.Node.div
      []
      [ allocations_line
      ; Vdom.Node.div [ Vdom.Attr.id "filter-graph-container" ] [ graph_node ]
      ; form
      ]
  in
  Vdom.Node.section
    [ Vdom.Attr.id "filter-panel" ]
    [ Vdom.Node.h2 [] [ header_text ]; main_body ]
;;

let time_view_holder =
  State_holder.component (module Graph_view.Time_view) ~initial:Elapsed_seconds
;;

let component
      ~graph
      ~filtered_graph
      ~filtered_allocations
      ~start_time
      ~inject_outgoing
      ~server_state
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub time_view_holder = time_view_holder in
  let time_view =
    let%map time_view_holder = time_view_holder in
    time_view_holder.current
  in
  let set_time_view =
    let%map time_view_holder = time_view_holder in
    time_view_holder.set
  in
  let%sub ranges = ranges ~graph ~start_time ~time_view in
  let%sub direction = direction_list in
  let%sub heap_filter = heap_filter in
  let allocated_range =
    let%map ranges = ranges in
    Range_input.range ranges.allocated
  in
  let live_range =
    let%map ranges = ranges in
    Range_input.range ranges.live
  in
  let%sub graph_view =
    graph_view
      ~graph
      ~filtered_graph
      ~allocated_range
      ~live_range
      ~start_time
      ~time_view
      ~set_time_view
  in
  let%sub button =
    button ~ranges ~direction ~heap_filter ~server_state ~inject_outgoing
  in
  return
    (let%map ranges = ranges
     and direction = direction
     and time_view = time_view
     and heap_filter = heap_filter
     and graph_node = graph_view
     and button_node, on_submit = button
     and server_state = server_state
     and filtered_allocations = filtered_allocations in
     let { Ranges.allocated; live } = ranges in
     let allocated_nodes =
       Range_input.lower_input allocated, Range_input.upper_input allocated
     in
     let live_nodes = Range_input.lower_input live, Range_input.upper_input live in
     let range_size = Range_input.size allocated in
     let direction_list_node = Radio_list.view direction in
     let heap_filter_node = Heap_filter.view heap_filter in
     panel
       ~server_state
       ~time_view
       ~filtered_allocations
       ~allocated_nodes
       ~live_nodes
       ~range_size
       ~graph_node
       ~direction_list_node
       ~heap_filter_node
       ~button_node
       ~on_submit)
;;
