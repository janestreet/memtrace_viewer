open! Core
open Bonsai_web_proc

module Size = struct
  type t =
    | Small
    | Large
  [@@deriving equal]
end

module Which_bound = struct
  type t =
    | Lower
    | Upper
end

type t = (Time_ns.Span.t option * Size.t) And_view.t

module Time_ns_span_option = struct
  type t = Time_ns.Span.t option [@@deriving equal, sexp]
end

let component ~which ~max ~start_time ~time_view =
  let open Bonsai.Let_syntax in
  let%sub state = Bonsai.state None ~equal:[%equal: Time_ns_span_option.t] in
  return
    (let%map value, set_value = state
     and max
     and start_time
     and time_view in
     let open Vdom in
     match time_view with
     | Graph_view.Time_view.Elapsed_seconds ->
       let max = max |> Time_ns.Span.to_sec in
       let step = Nice.round (max /. 100.) in
       (* Round the max up to the nearest step---it's nicer to look at and there's no harm
          done. *)
       let max_input =
         step *. Float.round_up (max /. step)
         |> Float.round_significant ~significant_digits:3
         (* There sholudn't be more than 3 significant digits that we care
            about---[Float.round_up (max_x /. step)] is a 2-digit number (or possibly 100)
            and step is [n *. 10. ** k] for some integer [k] and one-digit integer [n]. *)
       in
       let on_input new_value =
         set_value (new_value |> Option.map ~f:Time_ns.Span.of_sec)
       in
       let default_value =
         match which with
         | Which_bound.Lower -> 0.
         | Upper -> max_input
       in
       let value_seconds =
         match value with
         | None -> Some default_value
         | Some value -> Some (value |> Time_ns.Span.to_sec)
       in
       let placeholder = sprintf "%g" default_value in
       { And_view.value = value, Size.Small
       ; view =
           Node.span
             [ Vdom_input_widgets.Entry.number
                 ~allow_updates_when_focused:`Never
                 ~merge_behavior:Legacy_dont_merge
                 ~extra_attrs:
                   [ Attr.class_ "bound"
                   ; Attr.create_float "min" 0.
                   ; Attr.create_float "max" max_input
                   ]
                 ~call_on_input_when:Text_changed
                 ~value:value_seconds
                 ~placeholder
                 ~step
                 ~on_input
                 (module Util.Float_html_syntax)
             ; Node.text " s"
             ]
       }
     | Wall_time ->
       (* Note that this will work badly for small steps since we can't get more precise
          than seconds in a "datetime-local" input. (Actually the browser can go to
          milliseconds, but Vdom_input_widgets doesn't support it.) Wall time doesn't seem
          useful for such short-running traces, however. *)
       let step = Nice.Time_ns.Span.round (Time_ns.Span.scale max 0.01) in
       let start_day = Nice.Time_ns.start_of_day_utc start_time in
       let min_input =
         Nice.Time_ns.round_down_to_multiple_of_nice
           ~relative_to:start_day
           ~step
           start_time
       in
       let end_time = Time_ns.add start_time max in
       let max_input =
         Nice.Time_ns.round_up_to_multiple_of_nice ~relative_to:start_day ~step end_time
       in
       let on_input new_value =
         set_value (new_value |> Option.map ~f:(fun time -> Time_ns.diff time start_time))
       in
       let default_value =
         match which with
         | Which_bound.Lower -> min_input
         | Upper -> max_input
       in
       let abs_value =
         match value with
         | Some value -> Some (Time_ns.add start_time value)
         | None -> Some default_value
       in
       let zone = Time_float.Zone.utc in
       let same_day =
         Date.equal
           (min_input |> Time_ns.to_date ~zone)
           (max_input |> Time_ns.to_date ~zone)
       in
       (match same_day with
        | false ->
          let to_html_datetime time =
            (* Cribbed from Vdom_input_widgets *)
            let s = Time_ns.to_string_iso8601_basic ~zone time in
            String.lsplit2_exn ~on:'.' s |> Tuple2.get1
          in
          { value = value, Large
          ; view =
              Vdom_input_widgets.Entry.datetime_local
                ~allow_updates_when_focused:`Never
                ~merge_behavior:Legacy_dont_merge
                ~extra_attrs:
                  [ Attr.class_ "bound"
                  ; Attr.create "min" (min_input |> to_html_datetime)
                  ; Attr.create "max" (max_input |> to_html_datetime)
                  ; Attr.create_float "step" (step |> Time_ns.Span.to_sec)
                  ]
                ~call_on_input_when:Text_changed
                ~utc_offset:Time_ns.Span.zero
                ~value:abs_value
                ~on_input
                ()
          }
        | true ->
          let date = min_input |> Time_ns.to_date ~zone in
          let min_input = min_input |> Time_ns.to_ofday ~zone in
          let max_input = max_input |> Time_ns.to_ofday ~zone in
          let on_input ofday =
            on_input
              (ofday
               |> Option.map ~f:(fun ofday -> Time_ns.of_date_ofday ~zone date ofday))
          in
          let abs_value = abs_value |> Option.map ~f:(Time_ns.to_ofday ~zone) in
          { value = value, Size.Small
          ; view =
              Vdom_input_widgets.Entry.time
                ~allow_updates_when_focused:`Never
                ~merge_behavior:Legacy_dont_merge
                ~extra_attrs:
                  [ Attr.class_ "bound"
                  ; Attr.create "min" (min_input |> Time_ns.Ofday.to_millisecond_string)
                  ; Attr.create "max" (max_input |> Time_ns.Ofday.to_millisecond_string)
                  ; Attr.create_float "step" (step |> Time_ns.Span.to_sec)
                  ]
                ~call_on_input_when:Text_changed
                ~on_input
                ~value:abs_value
                ()
          }))
;;
