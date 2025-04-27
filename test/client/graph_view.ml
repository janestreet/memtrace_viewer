open! Core
open! Bonsai_web_proc
open Memtrace_viewer_common
open Memtrace_viewer_client.For_testing
module Handle = Bonsai_web_test.Handle

let mk_series ~start_y ~count ~step_x ~step_y ~css_class =
  let rec loop ~x ~y ~max_y ~count ~rev_acc =
    match count with
    | 0 ->
      let max_x = Time_ns.Span.(x - step_x) in
      assert (List.for_all rev_acc ~f:(fun (_, y) -> Byte_units.(y <= max_y)));
      let points = List.rev rev_acc in
      Graph_view.Series.create ~css_class ~max_x ~max_y points
    | _ ->
      let max_y = Byte_units.max y max_y in
      let rev_acc = (x, y) :: rev_acc in
      let x = Time_ns.Span.(x + step_x) in
      let y = Byte_units.(y + step_y) in
      let count = count - 1 in
      loop ~x ~y ~max_y ~count ~rev_acc
  in
  match count with
  | 0 ->
    Graph_view.Series.create ~css_class ~max_x:Time_ns.Span.zero ~max_y:Byte_units.zero []
  | _ when count < 0 -> assert false
  | _ -> loop ~x:Time_ns.Span.zero ~y:start_y ~max_y:Byte_units.zero ~count ~rev_acc:[]
;;

let get_vdom graph_view = graph_view

let mk_handle () =
  let series =
    let count = 20 in
    let step_x = 1 |> Time_ns.Span.of_int_sec in
    [ mk_series
        ~css_class:"series-a"
        ~start_y:Byte_units.zero
        ~step_y:(1. |> Byte_units.of_megabytes)
        ~count
        ~step_x
    ; mk_series
        ~css_class:"series-b"
        ~start_y:(30. |> Byte_units.of_megabytes)
        ~step_y:(-1.25 |> Byte_units.of_megabytes)
        ~count
        ~step_x
    ]
    |> Bonsai.Value.return
  in
  let regions =
    let module R = Range.Time_ns_span.Or_empty in
    let module T = Time_ns.Span in
    [ Graph_view.Region.create ~css_class:"region-a" (R.at_most (4. |> T.of_sec))
    ; Graph_view.Region.create ~css_class:"region-b" (R.at_least (17. |> T.of_sec))
    ]
    |> Bonsai.Value.return
  in
  let aspect_ratio = 2.0 |> Bonsai.Value.return in
  let start_time =
    "2022-01-01 12:00:00Z" |> Time_ns.of_string_with_utc_offset |> Bonsai.Value.return
  in
  let time_view_var = Bonsai.Var.create Graph_view.Time_view.Elapsed_seconds in
  let time_view = time_view_var |> Bonsai.Var.value in
  let set_time_view =
    (fun time_view -> Bonsai.Var.set time_view_var time_view)
    |> Effect.of_sync_fun
    |> Bonsai.Value.return
  in
  let graph_view =
    Graph_view.component
      ~series
      ~regions
      ~aspect_ratio
      ~start_time
      ~time_view
      ~set_time_view
  in
  Handle.create (Bonsai_web_test.Result_spec.vdom get_vdom) graph_view
;;

let%expect_test "default graph view" =
  let handle = mk_handle () in
  Handle.show handle;
  [%expect
    {|
    <div id="filter-graph-and-controls">
      <div id="filter-graph-container">
        <div id="filter-graph-sizer" size_tracker=<fun>>
          <svg id="filter-graph" viewBox="0 0 450 225" preserveAspectRatio="none" class="graph">
            <g x="0" y="0" width="390" height="195" class="graph-data">
              <rect x="50" y="5" width="390" height="195" class="graph-border"> </rect>
              <g>
                <polyline points="50,200 70.52631578947368,193.5 91.05263157894737,187 111.57894736842105,180.5 132.10526315789474,174 152.63157894736844,167.5 173.1578947368421,161 193.68421052631578,154.5 214.21052631578948,148 234.73684210526318,141.5 255.26315789473685,135 275.7894736842105,128.5 296.3157894736842,122 316.8421052631579,115.5 337.36842105263156,109 357.89473684210526,102.5 378.42105263157896,96 398.94736842105266,89.5 419.47368421052636,83 440,76.5"
                          class="graph-line series-a"> </polyline>
                <polyline points="50,5 70.52631578947368,13.125 91.05263157894737,21.25 111.57894736842105,29.375 132.10526315789474,37.5 152.63157894736844,45.625 173.1578947368421,53.75 193.68421052631578,61.875 214.21052631578948,70 234.73684210526318,78.125 255.26315789473685,86.25 275.7894736842105,94.375 296.3157894736842,102.5 316.8421052631579,110.625 337.36842105263156,118.75 357.89473684210526,126.875 378.42105263157896,135 398.94736842105266,143.125 419.47368421052636,151.25 440,159.375"
                          class="graph-line series-b"> </polyline>
              </g>
              <g>
                <line x1="91.05263157894737"
                      y1="190.25"
                      x2="91.05263157894737"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="132.10526315789474"
                      y1="190.25"
                      x2="132.10526315789474"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="173.1578947368421"
                      y1="190.25"
                      x2="173.1578947368421"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="214.21052631578948"
                      y1="190.25"
                      x2="214.21052631578948"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="255.26315789473685"
                      y1="190.25"
                      x2="255.26315789473685"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="296.3157894736842"
                      y1="190.25"
                      x2="296.3157894736842"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="337.36842105263156"
                      y1="190.25"
                      x2="337.36842105263156"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="378.42105263157896"
                      y1="190.25"
                      x2="378.42105263157896"
                      y2="200"
                      class="graph-tick-mark"> </line>
                <line x1="419.47368421052636"
                      y1="190.25"
                      x2="419.47368421052636"
                      y2="200"
                      class="graph-tick-mark"> </line>
              </g>
              <g>
                <line x1="59.75" y1="167.5" x2="50" y2="167.5" class="graph-tick-mark"> </line>
                <line x1="59.75" y1="135" x2="50" y2="135" class="graph-tick-mark"> </line>
                <line x1="59.75" y1="102.5" x2="50" y2="102.5" class="graph-tick-mark"> </line>
                <line x1="59.75" y1="70" x2="50" y2="70" class="graph-tick-mark"> </line>
                <line x1="59.75" y1="37.5" x2="50" y2="37.5" class="graph-tick-mark"> </line>
                <line x1="59.75" y1="5" x2="50" y2="5" class="graph-tick-mark"> </line>
              </g>
            </g>
            <g>
              <text x="50" y="215" class="graph-label graph-label-x"> 0 </text>
              <text x="91.05263157894737" y="215" class="graph-label graph-label-x"> 2 </text>
              <text x="132.10526315789474" y="215" class="graph-label graph-label-x"> 4 </text>
              <text x="173.1578947368421" y="215" class="graph-label graph-label-x"> 6 </text>
              <text x="214.21052631578948" y="215" class="graph-label graph-label-x"> 8 </text>
              <text x="255.26315789473685" y="215" class="graph-label graph-label-x"> 10 </text>
              <text x="296.3157894736842" y="215" class="graph-label graph-label-x"> 12 </text>
              <text x="337.36842105263156" y="215" class="graph-label graph-label-x"> 14 </text>
              <text x="378.42105263157896" y="215" class="graph-label graph-label-x"> 16 </text>
              <text x="419.47368421052636" y="215" class="graph-label graph-label-x"> 18 </text>
            </g>
            <g>
              <text x="45" y="200" class="graph-label graph-label-y"> 0B </text>
              <text x="45" y="167.5" class="graph-label graph-label-y"> 5.00M </text>
              <text x="45" y="135" class="graph-label graph-label-y"> 10.0M </text>
              <text x="45" y="102.5" class="graph-label graph-label-y"> 15.0M </text>
              <text x="45" y="70" class="graph-label graph-label-y"> 20.0M </text>
              <text x="45" y="37.5" class="graph-label graph-label-y"> 25.0M </text>
              <text x="45" y="5" class="graph-label graph-label-y"> 30.0M </text>
            </g>
            <g>
              <g class="graph-region region-a">
                <rect x="50"
                      y="5"
                      width="82.10526315789474"
                      height="195"
                      class="graph-region-interior"> </rect>
                <g> </g>
                <line x1="132.10526315789474"
                      y1="5"
                      x2="132.10526315789474"
                      y2="200"
                      class="graph-region-bound graph-region-bound-closed"> </line>
              </g>
              <g class="graph-region region-b">
                <rect x="398.94736842105266"
                      y="5"
                      width="41.05263157894734"
                      height="195"
                      class="graph-region-interior"> </rect>
                <line x1="398.94736842105266"
                      y1="5"
                      x2="398.94736842105266"
                      y2="200"
                      class="graph-region-bound graph-region-bound-closed"> </line>
                <g> </g>
              </g>
            </g>
          </svg>
        </div>
      </div>
      <div class="graph-x-axis-label">
        <select class="widget-dropdown" @on_change>
          <option value="0" #selected="true"> Elapsed time (s) </option>
          <option value="1" #selected="false"> Wall time </option>
        </select>
      </div>
    </div>
    |}]
;;

let%expect_test "change view to wall time" =
  let handle = mk_handle () in
  Handle.store_view handle;
  Handle.change handle ~selector:".graph-x-axis-label select" ~value:"1" ~get_vdom;
  Handle.show_diff handle;
  [%expect
    {|
      <div id="filter-graph-and-controls">
        <div id="filter-graph-container">
          <div id="filter-graph-sizer" size_tracker=<fun>>
            <svg id="filter-graph" viewBox="0 0 450 225" preserveAspectRatio="none" class="graph">
    -|        <g x="0" y="0" width="390" height="195" class="graph-data">
    +|        <g x="0" y="0" width="390" height="170" class="graph-data">
    -|          <rect x="50" y="5" width="390" height="195" class="graph-border"> </rect>
    +|          <rect x="50" y="5" width="390" height="170" class="graph-border"> </rect>
                <g>
    -|            <polyline points="50,200 70.52631578947368,193.5 91.05263157894737,187 111.57894736842105,180.5 132.10526315789474,174 152.63157894736844,167.5 173.1578947368421,161 193.68421052631578,154.5 214.21052631578948,148 234.73684210526318,141.5 255.26315789473685,135 275.7894736842105,128.5 296.3157894736842,122 316.8421052631579,115.5 337.36842105263156,109 357.89473684210526,102.5 378.42105263157896,96 398.94736842105266,89.5 419.47368421052636,83 440,76.5"
    +|            <polyline points="50,175 70.52631578947368,169.33333333333334 91.05263157894737,163.66666666666666 111.57894736842105,158 132.10526315789474,152.33333333333334 152.63157894736844,146.66666666666666 173.1578947368421,141 193.68421052631578,135.33333333333331 214.21052631578948,129.66666666666666 234.73684210526318,124 255.26315789473685,118.33333333333333 275.7894736842105,112.66666666666666 296.3157894736842,107 316.8421052631579,101.33333333333333 337.36842105263156,95.66666666666666 357.89473684210526,90 378.42105263157896,84.33333333333333 398.94736842105266,78.66666666666666 419.47368421052636,73 440,67.33333333333333"
                            class="graph-line series-a"> </polyline>
    -|            <polyline points="50,5 70.52631578947368,13.125 91.05263157894737,21.25 111.57894736842105,29.375 132.10526315789474,37.5 152.63157894736844,45.625 173.1578947368421,53.75 193.68421052631578,61.875 214.21052631578948,70 234.73684210526318,78.125 255.26315789473685,86.25 275.7894736842105,94.375 296.3157894736842,102.5 316.8421052631579,110.625 337.36842105263156,118.75 357.89473684210526,126.875 378.42105263157896,135 398.94736842105266,143.125 419.47368421052636,151.25 440,159.375"
    +|            <polyline points="50,5 70.52631578947368,12.083333333333314 91.05263157894737,19.166666666666657 111.57894736842105,26.25 132.10526315789474,33.333333333333314 152.63157894736844,40.41666666666666 173.1578947368421,47.5 193.68421052631578,54.58333333333333 214.21052631578948,61.66666666666666 234.73684210526318,68.75 255.26315789473685,75.83333333333333 275.7894736842105,82.91666666666666 296.3157894736842,90 316.8421052631579,97.08333333333333 337.36842105263156,104.16666666666666 357.89473684210526,111.25 378.42105263157896,118.33333333333333 398.94736842105266,125.41666666666666 419.47368421052636,132.5 440,139.58333333333331"
                            class="graph-line series-b"> </polyline>
                </g>
                <g>
                  <line x1="91.05263157894737"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="91.05263157894737"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="132.10526315789474"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="132.10526315789474"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="173.1578947368421"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="173.1578947368421"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="214.21052631578948"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="214.21052631578948"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="255.26315789473685"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="255.26315789473685"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="296.3157894736842"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="296.3157894736842"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="337.36842105263156"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="337.36842105263156"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="378.42105263157896"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="378.42105263157896"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                  <line x1="419.47368421052636"
    -|                  y1="190.25"
    +|                  y1="166.5"
                        x2="419.47368421052636"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-tick-mark"> </line>
                </g>
                <g>
    -|            <line x1="59.75" y1="167.5" x2="50" y2="167.5" class="graph-tick-mark"> </line>
    +|            <line x1="58.5"
    +|                  y1="146.66666666666666"
    +|                  x2="50"
    +|                  y2="146.66666666666666"
    +|                  class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="135" x2="50" y2="135" class="graph-tick-mark"> </line>
    +|            <line x1="58.5"
    +|                  y1="118.33333333333333"
    +|                  x2="50"
    +|                  y2="118.33333333333333"
    +|                  class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="102.5" x2="50" y2="102.5" class="graph-tick-mark"> </line>
    +|            <line x1="58.5" y1="90" x2="50" y2="90" class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="70" x2="50" y2="70" class="graph-tick-mark"> </line>
    +|            <line x1="58.5"
    +|                  y1="61.66666666666666"
    +|                  x2="50"
    +|                  y2="61.66666666666666"
    +|                  class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="37.5" x2="50" y2="37.5" class="graph-tick-mark"> </line>
    +|            <line x1="58.5"
    +|                  y1="33.333333333333314"
    +|                  x2="50"
    +|                  y2="33.333333333333314"
    +|                  class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="5" x2="50" y2="5" class="graph-tick-mark"> </line>
    +|            <line x1="58.5" y1="5" x2="50" y2="5" class="graph-tick-mark"> </line>
                </g>
              </g>
              <g>
    -|          <text x="50" y="215" class="graph-label graph-label-x"> 0 </text>
    +|          <text x="45"
    +|                y="190"
    +|                transform="rotate(20, 45, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00 </text>
    -|          <text x="91.05263157894737" y="215" class="graph-label graph-label-x"> 2 </text>
    +|          <text x="86.05263157894737"
    +|                y="190"
    +|                transform="rotate(20, 86.05263157894737, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:02 </text>
    -|          <text x="132.10526315789474" y="215" class="graph-label graph-label-x"> 4 </text>
    +|          <text x="127.10526315789474"
    +|                y="190"
    +|                transform="rotate(20, 127.10526315789474, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:04 </text>
    -|          <text x="173.1578947368421" y="215" class="graph-label graph-label-x"> 6 </text>
    +|          <text x="168.1578947368421"
    +|                y="190"
    +|                transform="rotate(20, 168.1578947368421, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:06 </text>
    -|          <text x="214.21052631578948" y="215" class="graph-label graph-label-x"> 8 </text>
    +|          <text x="209.21052631578948"
    +|                y="190"
    +|                transform="rotate(20, 209.21052631578948, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:08 </text>
    -|          <text x="255.26315789473685" y="215" class="graph-label graph-label-x"> 10 </text>
    +|          <text x="250.26315789473685"
    +|                y="190"
    +|                transform="rotate(20, 250.26315789473685, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:10 </text>
    -|          <text x="296.3157894736842" y="215" class="graph-label graph-label-x"> 12 </text>
    +|          <text x="291.3157894736842"
    +|                y="190"
    +|                transform="rotate(20, 291.3157894736842, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:12 </text>
    -|          <text x="337.36842105263156" y="215" class="graph-label graph-label-x"> 14 </text>
    +|          <text x="332.36842105263156"
    +|                y="190"
    +|                transform="rotate(20, 332.36842105263156, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:14 </text>
    -|          <text x="378.42105263157896" y="215" class="graph-label graph-label-x"> 16 </text>
    +|          <text x="373.42105263157896"
    +|                y="190"
    +|                transform="rotate(20, 373.42105263157896, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:16 </text>
    -|          <text x="419.47368421052636" y="215" class="graph-label graph-label-x"> 18 </text>
    +|          <text x="414.47368421052636"
    +|                y="190"
    +|                transform="rotate(20, 414.47368421052636, 190)"
    +|                class="graph-label graph-label-long graph-label-x"> 12:00:18 </text>
    -|        </g>
    -|        <g>
    -|          <text x="45" y="200" class="graph-label graph-label-y"> 0B </text>
    +|        </g>
    +|        <g>
    +|          <text x="45" y="175" class="graph-label graph-label-y"> 0B </text>
    -|          <text x="45" y="167.5" class="graph-label graph-label-y"> 5.00M </text>
    +|          <text x="45" y="146.66666666666666" class="graph-label graph-label-y"> 5.00M </text>
    -|          <text x="45" y="135" class="graph-label graph-label-y"> 10.0M </text>
    +|          <text x="45" y="118.33333333333333" class="graph-label graph-label-y"> 10.0M </text>
    -|          <text x="45" y="102.5" class="graph-label graph-label-y"> 15.0M </text>
    +|          <text x="45" y="90" class="graph-label graph-label-y"> 15.0M </text>
    -|          <text x="45" y="70" class="graph-label graph-label-y"> 20.0M </text>
    +|          <text x="45" y="61.66666666666666" class="graph-label graph-label-y"> 20.0M </text>
    -|          <text x="45" y="37.5" class="graph-label graph-label-y"> 25.0M </text>
    +|          <text x="45" y="33.333333333333314" class="graph-label graph-label-y"> 25.0M </text>
                <text x="45" y="5" class="graph-label graph-label-y"> 30.0M </text>
              </g>
              <g>
                <g class="graph-region region-a">
                  <rect x="50"
                        y="5"
                        width="82.10526315789474"
    -|                  height="195"
    +|                  height="170"
                        class="graph-region-interior"> </rect>
                  <g> </g>
                  <line x1="132.10526315789474"
                        y1="5"
                        x2="132.10526315789474"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-region-bound graph-region-bound-closed"> </line>
                </g>
                <g class="graph-region region-b">
                  <rect x="398.94736842105266"
                        y="5"
                        width="41.05263157894734"
    -|                  height="195"
    +|                  height="170"
                        class="graph-region-interior"> </rect>
                  <line x1="398.94736842105266"
                        y1="5"
                        x2="398.94736842105266"
    -|                  y2="200"
    +|                  y2="175"
                        class="graph-region-bound graph-region-bound-closed"> </line>
                  <g> </g>
                </g>
              </g>
            </svg>
          </div>
        </div>
        <div class="graph-x-axis-label">
          <select class="widget-dropdown" @on_change>
    -|      <option value="0" #selected="true"> Elapsed time (s) </option>
    +|      <option value="0" #selected="false"> Elapsed time (s) </option>
    -|      <option value="1" #selected="false"> Wall time </option>
    +|      <option value="1" #selected="true"> Wall time </option>
          </select>
        </div>
      </div>
    |}]
;;

let%expect_test "resize graph" =
  let handle = mk_handle () in
  Handle.store_view handle;
  Handle.trigger_hook
    handle
    ~selector:"#filter-graph-sizer"
    ~name:"size_tracker"
    Bonsai_web_ui_element_size_hooks.Size_tracker.For_testing.type_id
    { border_box = { width = 900.; height = 450. }
    ; content_box = { width = 900.; height = 450. }
    }
    ~get_vdom;
  Handle.show_diff handle;
  [%expect
    {|
      <div id="filter-graph-and-controls">
        <div id="filter-graph-container">
          <div id="filter-graph-sizer" size_tracker=<fun>>
    -|      <svg id="filter-graph" viewBox="0 0 450 225" preserveAspectRatio="none" class="graph">
    +|      <svg id="filter-graph" viewBox="0 0 900 450" preserveAspectRatio="none" class="graph">
    -|        <g x="0" y="0" width="390" height="195" class="graph-data">
    +|        <g x="0" y="0" width="840" height="420" class="graph-data">
    -|          <rect x="50" y="5" width="390" height="195" class="graph-border"> </rect>
    +|          <rect x="50" y="5" width="840" height="420" class="graph-border"> </rect>
                <g>
    -|            <polyline points="50,200 70.52631578947368,193.5 91.05263157894737,187 111.57894736842105,180.5 132.10526315789474,174 152.63157894736844,167.5 173.1578947368421,161 193.68421052631578,154.5 214.21052631578948,148 234.73684210526318,141.5 255.26315789473685,135 275.7894736842105,128.5 296.3157894736842,122 316.8421052631579,115.5 337.36842105263156,109 357.89473684210526,102.5 378.42105263157896,96 398.94736842105266,89.5 419.47368421052636,83 440,76.5"
    +|            <polyline points="50,425 94.21052631578948,411 138.42105263157896,397 182.6315789473684,383 226.8421052631579,369 271.0526315789474,355 315.2631578947368,341 359.4736842105263,327 403.6842105263158,313 447.89473684210526,299 492.10526315789474,285 536.3157894736842,271 580.5263157894736,257 624.7368421052631,243 668.9473684210526,229 713.1578947368421,215 757.3684210526316,201 801.578947368421,187 845.7894736842105,173 890,159"
                            class="graph-line series-a"> </polyline>
    -|            <polyline points="50,5 70.52631578947368,13.125 91.05263157894737,21.25 111.57894736842105,29.375 132.10526315789474,37.5 152.63157894736844,45.625 173.1578947368421,53.75 193.68421052631578,61.875 214.21052631578948,70 234.73684210526318,78.125 255.26315789473685,86.25 275.7894736842105,94.375 296.3157894736842,102.5 316.8421052631579,110.625 337.36842105263156,118.75 357.89473684210526,126.875 378.42105263157896,135 398.94736842105266,143.125 419.47368421052636,151.25 440,159.375"
    +|            <polyline points="50,5 94.21052631578948,22.5 138.42105263157896,40 182.6315789473684,57.5 226.8421052631579,75 271.0526315789474,92.5 315.2631578947368,110 359.4736842105263,127.5 403.6842105263158,145 447.89473684210526,162.5 492.10526315789474,180 536.3157894736842,197.5 580.5263157894736,215 624.7368421052631,232.5 668.9473684210526,250 713.1578947368421,267.5 757.3684210526316,285 801.578947368421,302.5 845.7894736842105,320 890,337.5"
                            class="graph-line series-b"> </polyline>
                </g>
                <g>
    -|            <line x1="91.05263157894737"
    +|            <line x1="138.42105263157896"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="91.05263157894737"
    +|                  x2="138.42105263157896"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="132.10526315789474"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="226.8421052631579"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="132.10526315789474"
    +|                  x2="226.8421052631579"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="173.1578947368421"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="315.2631578947368"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="173.1578947368421"
    +|                  x2="315.2631578947368"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="214.21052631578948"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="403.6842105263158"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="214.21052631578948"
    +|                  x2="403.6842105263158"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="255.26315789473685"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="492.10526315789474"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="255.26315789473685"
    +|                  x2="492.10526315789474"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="296.3157894736842"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="580.5263157894736"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="296.3157894736842"
    +|                  x2="580.5263157894736"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="337.36842105263156"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="668.9473684210526"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="337.36842105263156"
    +|                  x2="668.9473684210526"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="378.42105263157896"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="757.3684210526316"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="378.42105263157896"
    +|                  x2="757.3684210526316"
    -|                  y2="200"
    +|                  y2="425"
    -|                  class="graph-tick-mark"> </line>
    -|            <line x1="419.47368421052636"
    +|                  class="graph-tick-mark"> </line>
    +|            <line x1="845.7894736842105"
    -|                  y1="190.25"
    +|                  y1="404"
    -|                  x2="419.47368421052636"
    +|                  x2="845.7894736842105"
    -|                  y2="200"
    +|                  y2="425"
                        class="graph-tick-mark"> </line>
                </g>
                <g>
    -|            <line x1="59.75" y1="167.5" x2="50" y2="167.5" class="graph-tick-mark"> </line>
    +|            <line x1="71" y1="355" x2="50" y2="355" class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="135" x2="50" y2="135" class="graph-tick-mark"> </line>
    +|            <line x1="71" y1="285" x2="50" y2="285" class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="102.5" x2="50" y2="102.5" class="graph-tick-mark"> </line>
    +|            <line x1="71" y1="215" x2="50" y2="215" class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="70" x2="50" y2="70" class="graph-tick-mark"> </line>
    +|            <line x1="71" y1="145" x2="50" y2="145" class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="37.5" x2="50" y2="37.5" class="graph-tick-mark"> </line>
    +|            <line x1="71" y1="75" x2="50" y2="75" class="graph-tick-mark"> </line>
    -|            <line x1="59.75" y1="5" x2="50" y2="5" class="graph-tick-mark"> </line>
    +|            <line x1="71" y1="5" x2="50" y2="5" class="graph-tick-mark"> </line>
                </g>
              </g>
              <g>
    -|          <text x="50" y="215" class="graph-label graph-label-x"> 0 </text>
    +|          <text x="50" y="440" class="graph-label graph-label-x"> 0 </text>
    -|          <text x="91.05263157894737" y="215" class="graph-label graph-label-x"> 2 </text>
    +|          <text x="138.42105263157896" y="440" class="graph-label graph-label-x"> 2 </text>
    -|          <text x="132.10526315789474" y="215" class="graph-label graph-label-x"> 4 </text>
    +|          <text x="226.8421052631579" y="440" class="graph-label graph-label-x"> 4 </text>
    -|          <text x="173.1578947368421" y="215" class="graph-label graph-label-x"> 6 </text>
    +|          <text x="315.2631578947368" y="440" class="graph-label graph-label-x"> 6 </text>
    -|          <text x="214.21052631578948" y="215" class="graph-label graph-label-x"> 8 </text>
    +|          <text x="403.6842105263158" y="440" class="graph-label graph-label-x"> 8 </text>
    -|          <text x="255.26315789473685" y="215" class="graph-label graph-label-x"> 10 </text>
    +|          <text x="492.10526315789474" y="440" class="graph-label graph-label-x"> 10 </text>
    -|          <text x="296.3157894736842" y="215" class="graph-label graph-label-x"> 12 </text>
    +|          <text x="580.5263157894736" y="440" class="graph-label graph-label-x"> 12 </text>
    -|          <text x="337.36842105263156" y="215" class="graph-label graph-label-x"> 14 </text>
    +|          <text x="668.9473684210526" y="440" class="graph-label graph-label-x"> 14 </text>
    -|          <text x="378.42105263157896" y="215" class="graph-label graph-label-x"> 16 </text>
    +|          <text x="757.3684210526316" y="440" class="graph-label graph-label-x"> 16 </text>
    -|          <text x="419.47368421052636" y="215" class="graph-label graph-label-x"> 18 </text>
    +|          <text x="845.7894736842105" y="440" class="graph-label graph-label-x"> 18 </text>
    -|        </g>
    -|        <g>
    -|          <text x="45" y="200" class="graph-label graph-label-y"> 0B </text>
    +|        </g>
    +|        <g>
    +|          <text x="45" y="425" class="graph-label graph-label-y"> 0B </text>
    -|          <text x="45" y="167.5" class="graph-label graph-label-y"> 5.00M </text>
    +|          <text x="45" y="355" class="graph-label graph-label-y"> 5.00M </text>
    -|          <text x="45" y="135" class="graph-label graph-label-y"> 10.0M </text>
    +|          <text x="45" y="285" class="graph-label graph-label-y"> 10.0M </text>
    -|          <text x="45" y="102.5" class="graph-label graph-label-y"> 15.0M </text>
    +|          <text x="45" y="215" class="graph-label graph-label-y"> 15.0M </text>
    -|          <text x="45" y="70" class="graph-label graph-label-y"> 20.0M </text>
    +|          <text x="45" y="145" class="graph-label graph-label-y"> 20.0M </text>
    -|          <text x="45" y="37.5" class="graph-label graph-label-y"> 25.0M </text>
    +|          <text x="45" y="75" class="graph-label graph-label-y"> 25.0M </text>
                <text x="45" y="5" class="graph-label graph-label-y"> 30.0M </text>
              </g>
              <g>
                <g class="graph-region region-a">
                  <rect x="50"
                        y="5"
    -|                  width="82.10526315789474"
    +|                  width="176.8421052631579"
    -|                  height="195"
    +|                  height="420"
                        class="graph-region-interior"> </rect>
                  <g> </g>
    -|            <line x1="132.10526315789474"
    +|            <line x1="226.8421052631579"
                        y1="5"
    -|                  x2="132.10526315789474"
    +|                  x2="226.8421052631579"
    -|                  y2="200"
    +|                  y2="425"
                        class="graph-region-bound graph-region-bound-closed"> </line>
                </g>
                <g class="graph-region region-b">
    -|            <rect x="398.94736842105266"
    +|            <rect x="801.578947368421"
                        y="5"
    -|                  width="41.05263157894734"
    +|                  width="88.42105263157896"
    -|                  height="195"
    +|                  height="420"
                        class="graph-region-interior"> </rect>
    -|            <line x1="398.94736842105266"
    +|            <line x1="801.578947368421"
                        y1="5"
    -|                  x2="398.94736842105266"
    +|                  x2="801.578947368421"
    -|                  y2="200"
    +|                  y2="425"
                        class="graph-region-bound graph-region-bound-closed"> </line>
                  <g> </g>
                </g>
              </g>
            </svg>
          </div>
        </div>
        <div class="graph-x-axis-label">
          <select class="widget-dropdown" @on_change>
            <option value="0" #selected="true"> Elapsed time (s) </option>
            <option value="1" #selected="false"> Wall time </option>
          </select>
        </div>
      </div>
    |}]
;;
