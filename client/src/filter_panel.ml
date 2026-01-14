open! Core
open! Bonsai_web_proc
module Attr = Vdom.Attr
module Node = Vdom.Node
module Node_svg = Virtual_dom_svg.Node
open Memtrace_viewer_common

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
  let%sub series =
    let%arr graph and filtered_graph in
    let max_x =
      match filtered_graph with
      | None -> Data.Graph.max_x graph
      | Some filtered_graph ->
        Time_ns.Span.max (Data.Graph.max_x graph) (Data.Graph.max_x filtered_graph)
    in
    let series_of_data_graph css_class (graph : Data.Graph.t) =
      let max_y = Data.Graph.max_y graph in
      let points_rev = List.rev (Data.Graph.points graph) in
      (* Make sure the graph has points at x=0 and x=max_x so that it ranges through the
         whole timeline. We can assume that the total allocations start at 0 and don't
         change after the last reported point. *)
      let last_x, last_y =
        match points_rev with
        | last_point :: _ -> last_point
        | [] -> Time_ns.Span.zero, Byte_units.zero
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
        | _ -> (Time_ns.Span.zero, Byte_units.zero) :: points
      in
      Graph_view.Series.create ~css_class ~max_x ~max_y points
    in
    let full_series = series_of_data_graph "full-graph-line" graph in
    let filtered_series =
      Option.map ~f:(series_of_data_graph "filtered-graph-line") filtered_graph
    in
    List.filter_opt [ filtered_series; Some full_series ]
  in
  let%sub regions =
    let%arr allocated_range and collected_range in
    let region_of_range css_class range : Graph_view.Region.t =
      Graph_view.Region.create ~css_class range
    in
    let allocated_region =
      region_of_range "graph-allocated-range" (Non_empty allocated_range)
    in
    let collected_region = region_of_range "graph-collected-range" collected_range in
    [ allocated_region; collected_region ]
  in
  let aspect_ratio = Value.return 2.0 in
  Graph_view.component
    ~series
    ~regions
    ~start_time
    ~time_view
    ~set_time_view
    ~aspect_ratio
;;

module Submission_handling = struct
  type t =
    { button : Vdom.Node.t
    ; on_submit : unit Vdom.Effect.t
    }

  let component ~server_state ~inject_outgoing ~filter =
    let open Bonsai.Let_syntax in
    return
      (let%map server_state and inject_outgoing and filter in
       let do_submit =
         match filter with
         | Some filter -> inject_outgoing (Action.Set_filter filter)
         | None -> Vdom.Effect.Ignore
       in
       let on_submit =
         Vdom.Effect.Many
           [ Vdom.Effect.Prevent_default (* don't do a real HTML submit! *); do_submit ]
       in
       let server_is_idle =
         Server_state.Status.is_idle Server_state.(server_state.status)
       in
       let filter_is_valid = Option.is_some filter in
       let enabled = server_is_idle && filter_is_valid in
       let button =
         Node.input
         (* Don't actually put an onclick handler on the button; just return the handler
            to be used as the form's onsubmit instead, thus getting Enter key behavior for
            free
         *)
           ~attrs:
             [ Attr.type_ "submit"
             ; Attr.class_ "flat-button"
             ; Attr.value "Apply"
             ; Attr.title "Set current filter and redraw views"
             ; (if not enabled then Attr.disabled else Attr.empty)
             ]
           ()
       in
       { button; on_submit })
  ;;
end

let panel_body
  ~server_state
  ~total_allocations
  ~filtered_allocations
  ~peak_allocations
  ~(filter : Filter.t)
  ~filter_clauses
  ~graph_node
  ~button_node
  ~on_submit
  =
  let allocations_line desc bytes =
    Node.p
      ~attrs:[ Attr.class_ "total-allocations" ]
      [ Node.textf "%s: %s" desc (bytes |> Byte_units.Short.to_string) ]
  in
  let total_allocations_line = allocations_line "Total allocations" total_allocations in
  let peak_allocations_line = allocations_line "Peak live memory" peak_allocations in
  let filtered_allocations_line =
    match filtered_allocations with
    | None -> Util.placeholder_div
    | Some bytes -> allocations_line "Filtered allocations" bytes
  in
  let swatch swatch_class =
    Node_svg.svg
      ~attrs:[ Attr.classes [ "swatch"; swatch_class ] ]
      [ Node_svg.rect ~attrs:[ Attr.class_ "swatch-bg" ] []
      ; Node_svg.rect ~attrs:[ Attr.class_ "swatch-interior" ] []
      ; Node_svg.rect ~attrs:[ Attr.class_ "swatch-border" ] []
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
    else Node.p region_legend_text
  in
  let node_server_state =
    match server_state.Server_state.status with
    | Down -> Codicons.svg Debug_disconnect
    | Busy -> Codicons.svg ~extra_attrs:[ Attr.class_ "spinner" ] Loading
    | Idle -> Node.none_deprecated [@alert "-deprecated"]
  in
  let form =
    Node.create
      "form"
      ~attrs:
        [ Attr.id "filter-form"
        ; Attr.on_submit (fun _ -> on_submit)
        ; (* Browser-level validation isn't our friend - it rejects numeric inputs if the
             user inputs too many digits (as decided by the step) *)
          Attr.create "novalidate" ""
        ; (* This effectively disables the table or flame graph's keyboard event handler
             whenever the focus is anywhere in the form, so that Enter, arrow keys, etc.
             all work correcly *)
          Attr.on_keydown (fun _ -> Vdom.Effect.Stop_propagation)
        ]
      [ region_legend
      ; And_view.view filter_clauses
      ; Node.div [ button_node; node_server_state ]
      ]
  in
  Node.div
    ~attrs:[ Attr.id "filter-panel-body" ]
    [ graph_node
    ; Node.div
        ~attrs:[ Attr.id "filter-form-area" ]
        [ total_allocations_line; peak_allocations_line; filtered_allocations_line; form ]
    ]
;;

let time_view_holder =
  Bonsai.state Elapsed_seconds ~equal:[%equal: Graph_view.Time_view.t]
;;

let component
  ~graph
  ~filtered_graph
  ~total_allocations
  ~filtered_allocations
  ~peak_allocations
  ~peak_allocations_time
  ~start_time
  ~inject_outgoing
  ~server_state
  : Vdom.Node.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub time_view_holder in
  let time_view = Bonsai.Value.map ~f:fst time_view_holder in
  let set_time_view = Bonsai.Value.map ~f:snd time_view_holder in
  let max_time =
    let%map graph in
    Data.Graph.max_x graph
  in
  let%sub filter_clauses = Filter_editor.component ~max_time ~start_time ~time_view in
  let filter_spec : Filter_spec.t Bonsai.Value.t =
    let%map filter_clauses in
    Filter_spec.{ clauses = filter_clauses.value }
  in
  (* If the filter is incomplete (that is, there are blank entries), we don't want to
     enable Apply but we do want to show as much of the filter as makes sense. This is
     especially important because otherwise adding a new clause would cause the displayed
     filter (e.g., the ranges in the graph) to disappear.

     (We could instead use a component that remembers the last valid filter, but that's
     rather involved. We would want something (more or less) of type

     [is_valid:('a -> bool) -> 'a Bonsai.Value.t -> 'a Bonsai.Computation.t]

     but that won't work because Bonsai computations are pure---it can't store the last
     known valid input as a side effect. It could instead be in the style of a state
     machine:

     [is_valid:('a -> bool) -> ('a * ('a -> unit Vdom.Effect.t)) Bonsai.Computation.t]

     but now, to fire that event, all the components in the filter editor would have to
     support an on_changed event, which is a much more painful workaround than just having
     this [to_filter_allow_incomplete] function.) *)
  let%sub filter_to_display =
    let%arr filter_spec and peak_allocations_time in
    Filter_spec.to_filter_allow_incomplete filter_spec ~peak_allocations_time
  in
  let%sub complete_filter =
    let%arr filter_spec and peak_allocations_time in
    Filter_spec.to_filter filter_spec ~peak_allocations_time
  in
  let allocated_range =
    let%map filter_to_display in
    filter_to_display.allocated_range
  in
  let collected_range =
    let%map filter_to_display in
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
  let panel_body =
    let%map filter = filter_to_display
    and filter_clauses
    and graph_node = graph_view
    and { button = button_node; on_submit } = submission_handling
    and server_state
    and total_allocations
    and filtered_allocations
    and peak_allocations in
    panel_body
      ~server_state
      ~total_allocations
      ~filtered_allocations
      ~peak_allocations
      ~filter
      ~filter_clauses
      ~graph_node
      ~button_node
      ~on_submit
  in
  Panel.panel
    ~title:(Value.return "Filter")
    ~id:"filter-panel"
    ~collapsible:(Yes { initial_state = Expanded })
    panel_body
;;
