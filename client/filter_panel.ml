open! Core_kernel
open! Bonsai_web
module Attr = Vdom.Attr
module Node = Vdom.Node
module Node_svg = Virtual_dom_svg.Node
module Attr_svg = Virtual_dom_svg.Attr
open Memtrace_viewer_common

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

let graph_view
      ~graph
      ~filtered_graph
      ~allocated_range
      ~collected_range
      ~start_time
      ~time_view
      ~set_time_view
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let width = 500 in
  let height = 200 in
  let series =
    let%map graph = graph
    and filtered_graph = filtered_graph in
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
    List.filter_opt [ filtered_series; Some full_series ]
  in
  let regions =
    let%map allocated_range = allocated_range
    and collected_range = collected_range in
    let region_of_range css_class range : Graph_view.Region.t =
      Graph_view.Region.create ~css_class range
    in
    let allocated_region =
      region_of_range "graph-allocated-range" (Non_empty allocated_range)
    in
    let collected_region = region_of_range "graph-collected-range" collected_range in
    [ allocated_region; collected_region ]
  in
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

module Submission_handling = struct
  type t =
    { button : Vdom.Node.t
    ; on_submit : Vdom.Event.t
    }

  let component ~server_state ~inject_outgoing ~filter =
    let open Bonsai.Let_syntax in
    return
      (let%map server_state = server_state
       and inject_outgoing = inject_outgoing
       and filter = filter in
       let do_submit =
         match filter with
         | Some filter ->
           inject_outgoing (Action.Set_filter filter)
         | None -> Vdom.Event.Ignore
       in
       let on_submit =
         Vdom.Event.Many
           [ Vdom.Event.Prevent_default (* don't do a real HTML submit! *); do_submit ]
       in
       let server_is_up = Server_state.Status.is_up Server_state.(server_state.status) in
       let filter_is_valid = Option.is_some filter in
       let enabled = server_is_up && filter_is_valid in
       let button =
         Node.input
           (* Don't actually put an onclick handler on the button; just return the handler
              to be used as the form's onsubmit instead, thus getting Enter key behavior
              for free
           *)
           ([ Attr.type_ "submit"
            ; Attr.value "Apply"
            ; Attr.title "Set current filter and redraw views"
            ]
            @ if not enabled then [ Attr.disabled ] else [])
           []
       in
       { button; on_submit })
  ;;
end

let panel
      ~server_state
      ~filtered_allocations
      ~(filter : Filter.t)
      ~filter_clauses
      ~graph_node
      ~direction_list_node
      ~button_node
      ~on_submit
  =
  let allocations_line =
    match filtered_allocations with
    | None -> Util.placeholder_svg
    | Some bytes ->
      Node.p
        [ Attr.class_ "total-allocations" ]
        [ Node.textf "Filtered allocations: %s" (bytes |> Byte_units.Short.to_string) ]
  in
  let swatch swatch_class =
    Node_svg.svg
      [ Attr.classes [ "swatch"; swatch_class ] ]
      [ Node_svg.rect [ Attr.class_ "swatch-bg" ] []
      ; Node_svg.rect [ Attr.class_ "swatch-interior" ] []
      ; Node_svg.rect [ Attr.class_ "swatch-border" ] []
      ]
  in
  let region_legend_text =
    let phrase swatch_class desc range =
      if Range.Time_ns_span.is_all range
      then None
      else Some [ Node.text " "; swatch swatch_class; Node.textf "%s in this range" desc ]
    in
    let allocated_phrase = phrase "allocated-swatch" "allocated" filter.allocated_range in
    let collected_phrase =
      match filter.collected_range with
      | Non_empty range -> phrase "collected-swatch" "collected" range
      | Empty -> Some [ Node.textf " never collected" ]
    in
    let initial_fragment = Node.text "Showing objects that are" in
    match allocated_phrase, collected_phrase with
    | None, None -> []
    | Some phrase, None | None, Some phrase -> initial_fragment :: phrase
    | Some phrase1, Some phrase2 ->
      List.concat [ [ initial_fragment ]; phrase1; [ Node.text " and" ]; phrase2 ]
  in
  let region_legend =
    if List.is_empty region_legend_text
    then Util.placeholder_div
    else Node.p [] region_legend_text
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
      [ region_legend
      ; And_view.view filter_clauses
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
  Bonsai.state [%here] (module Graph_view.Time_view) ~default_model:Elapsed_seconds
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
  let time_view = Bonsai.Value.map ~f:fst time_view_holder in
  let set_time_view = Bonsai.Value.map ~f:snd time_view_holder in
  let max_time =
    let%map graph = graph in
    Data.Graph.max_x graph
  in
  let%sub filter_clauses = Filter_editor.component ~max_time ~start_time ~time_view in
  let%sub direction = direction_list in
  let filter_spec : Filter_spec.t Bonsai.Value.t =
    let%map filter_clauses = filter_clauses
    and direction = direction in
    Filter_spec.{ clauses = filter_clauses.value; direction = direction.value }
  in
  (* If the filter is incomplete (that is, there are blank entries), we don't want to
     enable Apply but we do want to show as much of the filter as makes sense. This is
     especially important because otherwise adding a new clause would cause the displayed
     filter (e.g., the ranges in the graph) to disappear.

     (We could instead use a component that remembers the last valid filter, but that's
     rather involved. We would want something (more or less) of type

     [is_valid:('a -> bool) -> 'a Bonsai.Value.t -> 'a Bonsai.Computation.t]

     but that won't work because Bonsai computations are pure---it can't store the
     last known valid input as a side effect. It could instead be in the style of a
     state machine:

     [is_valid:('a -> bool) -> ('a * ('a -> Vdom.Event.t)) Bonsai.Computation.t]

     but now, to fire that event, all the components in the filter editor would have to
     support an on_changed event, which is a much more painful workaround than just
     having this [to_filter_allow_incomplete] function.) *)
  let filter_to_display =
    Bonsai.Value.map ~f:Filter_spec.to_filter_allow_incomplete filter_spec
  in
  let complete_filter = Bonsai.Value.map ~f:Filter_spec.to_filter filter_spec in
  let allocated_range =
    let%map filter_to_display = filter_to_display in
    filter_to_display.allocated_range
  in
  let collected_range =
    let%map filter_to_display = filter_to_display in
    filter_to_display.collected_range
  in
  let%sub graph_view =
    graph_view
      ~graph
      ~filtered_graph
      ~allocated_range
      ~collected_range
      ~start_time
      ~time_view
      ~set_time_view
  in
  let%sub submission_handling =
    Submission_handling.component ~server_state ~inject_outgoing ~filter:complete_filter
  in
  return
    (let%map filter = filter_to_display
     and direction = direction
     and filter_clauses = filter_clauses
     and graph_node = graph_view
     and { button = button_node; on_submit } = submission_handling
     and server_state = server_state
     and filtered_allocations = filtered_allocations in
     let direction_list_node = direction.view in
     panel
       ~server_state
       ~filtered_allocations
       ~filter
       ~filter_clauses
       ~graph_node
       ~direction_list_node
       ~button_node
       ~on_submit)
;;
