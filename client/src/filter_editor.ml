open! Core
open Bonsai_web_proc

type t = Filter_spec.Clause.t option list And_view.t

module Which_bound = Time_bound_input.Which_bound

let space = Vdom.Node.text " "

module Time_parameters = struct
  type t =
    { max_time : Time_ns.Span.t
    ; start_time : Time_ns.t
    ; time_view : Graph_view.Time_view.t
    }
  [@@deriving fields ~getters]
end

let map_component ~f c =
  let open Bonsai.Let_syntax in
  let%sub c in
  return (Bonsai.Value.map ~f c)
;;

let map_component_value ~f c = map_component ~f:(And_view.map ~f) c
let map_component_view ~f c = map_component ~f:(And_view.map_view ~f) c

module Editor = struct
  type 'a t =
    time_parameters:Time_parameters.t Bonsai.Value.t
    -> 'a option And_view.t Bonsai.Computation.t

  let map ~f (t : 'a t) : 'b t =
    fun ~time_parameters -> map_component_value ~f (t ~time_parameters)
  ;;

  let map_view ~f (t : 'a t) : 'b t =
    fun ~time_parameters -> map_component_view ~f (t ~time_parameters)
  ;;

  let alter_parameters ~f (t : 'a t) : 'a t =
    fun ~time_parameters ->
    let time_parameters = Bonsai.Value.map ~f time_parameters in
    t ~time_parameters
  ;;
end

(** An "editor" that puts nothing on the page and simply returns a constant value *)
let const_editor (value : 'a) : 'a Editor.t =
  fun ~time_parameters:_ ->
  Bonsai.const { And_view.value = Some value; view = Vdom.Node.none }
;;

(** A module that can be passed either to [Bonsai.enum] or to
    [Vdom_input_widgets.Dropdown.of_enum] *)
module type Enum = sig
  type t [@@deriving equal, compare, enumerate, sexp]

  val to_string : t -> string
end

module Option_of_enum (Enum : Enum) = struct
  type t = Enum.t option [@@deriving equal, compare, enumerate, sexp]
end

let dropdown_of_enum_opt (type enum) (module Enum : Enum with type t = enum)
  : enum option And_view.t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  let%sub state = Bonsai.state None ~equal:[%equal: Enum.t option] in
  return
    (let%map value, set_value = state in
     let view =
       Vdom_input_widgets.Dropdown.of_enum_opt
         (module Enum)
         ~selected:value
         ~on_change:set_value
     in
     { And_view.value; view })
;;

let phrase_editor
  (type head)
  (module Head : Enum with type t = head)
  ~(body_editor : Head.t -> 'phrase Editor.t)
  : 'phrase Editor.t
  =
  fun ~time_parameters ->
  let open Bonsai.Let_syntax in
  let%sub type_dropdown = dropdown_of_enum_opt (module Head) in
  let%sub rest_of_sentence =
    let type_ = Bonsai.Value.map ~f:And_view.value type_dropdown in
    Bonsai.enum (module Option_of_enum (Head)) ~match_:type_ ~with_:(fun type_ ->
      match type_ with
      | None ->
        Bonsai.const
          { And_view.view = Vdom.Node.none_deprecated [@alert "-deprecated"]
          ; value = None
          }
      | Some type_ -> body_editor type_ ~time_parameters)
  in
  return
    (let%map type_dropdown and rest_of_sentence in
     { And_view.value = rest_of_sentence.value
     ; view = Vdom.Node.span [ type_dropdown.view; space; rest_of_sentence.view ]
     })
;;

let enum_editor (type enum) (module Enum : Enum with type t = enum) : enum Editor.t =
  phrase_editor (module Enum) ~body_editor:(fun enum -> const_editor enum)
;;

module Range_predicate_head = struct
  module T = struct
    type t =
      | LE
      | GE
      | Between
    [@@deriving equal, compare, enumerate, sexp]
  end

  include T

  module With_time_phrasing = struct
    include T

    let to_string = function
      | LE -> "at or before"
      | GE -> "at or after"
      | Between -> "between"
    ;;
  end

  module With_number_phrasing = struct
    include T

    let to_string = function
      | LE -> "at most"
      | GE -> "at least"
      | Between -> "between"
    ;;
  end
end

open Bonsai.Let_syntax

let range_body_editor
  ~(bound_editor : which_bound:Which_bound.t -> 'point Editor.t)
  (head : Range_predicate_head.t)
  : 'point Filter_spec.Range_predicate.t Editor.t
  =
  match head with
  | LE ->
    bound_editor ~which_bound:Lower
    |> Editor.map ~f:(fun bound -> Some (Filter_spec.Range_predicate.At_most bound))
  | GE ->
    bound_editor ~which_bound:Upper
    |> Editor.map ~f:(fun bound -> Some (Filter_spec.Range_predicate.At_least bound))
  | Between ->
    fun ~time_parameters ->
      let%sub lower_bound = bound_editor ~which_bound:Lower ~time_parameters in
      let%sub upper_bound = bound_editor ~which_bound:Upper ~time_parameters in
      return
        (let%map lower_bound and upper_bound in
         let view =
           Vdom.Node.span [ lower_bound.view; Vdom.Node.text " and "; upper_bound.view ]
         in
         { And_view.value =
             Some
               (Filter_spec.Range_predicate.Between
                  { lower_bound = lower_bound.value; upper_bound = upper_bound.value })
         ; view
         })
;;

let time_editor ~which_bound : Time_ns.Span.t Editor.t =
  fun ~time_parameters ->
  let open Bonsai.Value.Applicative_infix in
  let max_time = time_parameters >>| Time_parameters.max_time in
  let start_time = time_parameters >>| Time_parameters.start_time in
  let time_view = time_parameters >>| Time_parameters.time_view in
  Time_bound_input.component ~which:which_bound ~max:max_time ~start_time ~time_view
  |> map_component_value ~f:fst
;;

let time_range_body_editor head = range_body_editor head ~bound_editor:time_editor

let duration_range_body_editor head =
  (* Same as editing a time range, only we pretend the view is always elapsed seconds *)
  time_range_body_editor head
  |> Editor.alter_parameters ~f:(fun time_parameters ->
    { time_parameters with time_view = Elapsed_seconds })
;;

let allocated_clause_editor : Filter_spec.Clause.t Editor.t =
  phrase_editor
    (module Range_predicate_head.With_time_phrasing)
    ~body_editor:time_range_body_editor
  |> Editor.map ~f:(fun range : Filter_spec.Clause.t option -> Some (Allocated range))
;;

module Live_clause_head = struct
  type t =
    | In_range of Range_predicate_head.t
    | At
    | At_end
    | At_peak
  [@@deriving sexp, enumerate, compare, equal]

  let to_string = function
    | In_range range -> Range_predicate_head.With_time_phrasing.to_string range
    | At -> "at"
    | At_end -> "at end of trace"
    | At_peak -> "at peak"
  ;;
end

let live_clause_editor : Filter_spec.Clause.t Editor.t =
  phrase_editor (module Live_clause_head) ~body_editor:(function
    | In_range head ->
      time_range_body_editor head
      |> Editor.map
           ~f:
             (Option.map ~f:(fun range ->
                Filter_spec.Clause.Live (Some (Anywhere_in_range range))))
    | At ->
      time_editor ~which_bound:Lower
      |> Editor.map ~f:(fun time -> Some (Filter_spec.Clause.Live (Some (At time))))
    | At_end -> const_editor (Filter_spec.Clause.Live (Some At_end_of_trace))
    | At_peak -> const_editor (Filter_spec.Clause.Live (Some At_peak_allocations)))
;;

module Collected_clause_head = struct
  type t =
    | In_range of Range_predicate_head.t
    | Never
  [@@deriving sexp, enumerate, compare, equal]

  let to_string = function
    | In_range range -> Range_predicate_head.With_time_phrasing.to_string range
    | Never -> "never"
  ;;
end

let collected_clause_editor =
  phrase_editor (module Collected_clause_head) ~body_editor:(function
    | In_range head ->
      time_range_body_editor head
      |> Editor.map
           ~f:
             (Option.map ~f:(fun range ->
                Filter_spec.Clause.Collected (Some (Non_empty range))))
    | Never -> const_editor (Filter_spec.Clause.Collected (Some Empty)))
;;

let size_clause_editor : Filter_spec.Clause.t Editor.t =
  phrase_editor
    (module Range_predicate_head.With_number_phrasing)
    ~body_editor:
      (range_body_editor ~bound_editor:(fun ~which_bound:_ ~time_parameters:_ ->
         Size_input.component))
  |> Editor.map ~f:(fun range -> Some (Filter_spec.Clause.Size range))
;;

let lifetime_clause_editor : Filter_spec.Clause.t Editor.t =
  phrase_editor
    (module Range_predicate_head.With_number_phrasing)
    ~body_editor:duration_range_body_editor
  |> Editor.map ~f:(fun range : Filter_spec.Clause.t option -> Some (Lifetime range))
;;

let string_editor ~time_parameters:_ = String_input.component

module String_clause_head = struct
  type t =
    | Equals
    | Contains
  [@@deriving sexp, enumerate, compare, equal]

  let to_string = function
    | Equals -> "equals"
    | Contains -> "contains"
  ;;
end

let function_clause_editor ~clause_ctor ~text_before : Filter_spec.Clause.t Editor.t =
  phrase_editor (module String_clause_head) ~body_editor:(fun relation ->
    string_editor
    |> Editor.map ~f:(fun rhs : Filter_spec.Clause.t option ->
      let pred : Filter_spec.String_predicate.t =
        match relation with
        | Equals -> Equals rhs
        | Contains -> Contains rhs
      in
      Some (clause_ctor (Some pred))))
  |> Editor.map_view ~f:(fun node ->
    Vdom.Node.span [ Vdom.Node.textf "%s " text_before; node ])
;;

let inside_clause_editor : Filter_spec.Clause.t Editor.t =
  function_clause_editor ~text_before:"a function whose name" ~clause_ctor:(fun pred ->
    Require_function pred)
;;

let not_inside_clause_editor : Filter_spec.Clause.t Editor.t =
  function_clause_editor ~text_before:"a function whose name" ~clause_ctor:(fun pred ->
    Forbid_function pred)
;;

module Area = struct
  type t = Filter_spec.Area.t =
    | Major
    | Minor
    | External
  [@@deriving sexp, enumerate, compare, equal]

  let to_string = function
    | Major -> "major"
    | Minor -> "minor"
    | External -> "external"
  ;;
end

let area_clause_editor ~(predicate : Area.t -> Filter_spec.Area_predicate.t)
  : Filter_spec.Clause.t Editor.t
  =
  enum_editor (module Area)
  |> Editor.map ~f:(fun area : Filter_spec.Clause.t option ->
    Option.map area ~f:(fun area : Filter_spec.Clause.t -> Area (Some (predicate area))))
;;

let on_heap_clause_editor : Filter_spec.Clause.t Editor.t =
  area_clause_editor ~predicate:(fun area -> Is area)
;;

let not_on_heap_clause_editor : Filter_spec.Clause.t Editor.t =
  area_clause_editor ~predicate:(fun area -> Is_not area)
;;

module Toplevel_clause_head = struct
  type t =
    | Allocated
    | Live
    | Collected
    | Size
    | Lifetime
    | Some_function_name
    | No_function_name
    | On_heap
    | Not_on_heap
  [@@deriving enumerate, equal, compare, sexp]

  let to_string = function
    | Allocated -> "allocated"
    | Live -> "live"
    | Collected -> "collected"
    | Size -> "whose size is"
    | Lifetime -> "whose lifetime is"
    | Some_function_name -> "inside a call to"
    | No_function_name -> "not inside a call to"
    | On_heap -> "on the _ heap"
    | Not_on_heap -> "not on the _ heap"
  ;;
end

let with_conjunction conjunction view =
  let open Vdom in
  Node.span
    [ Node.span ~attrs:[ Attr.class_ "conjunction" ] [ Node.textf "%s " conjunction ]
    ; view
    ]
;;

let toplevel_clause_editor =
  phrase_editor (module Toplevel_clause_head) ~body_editor:(function
    | Allocated -> allocated_clause_editor
    | Live -> live_clause_editor
    | Collected -> collected_clause_editor
    | Size -> size_clause_editor
    | Lifetime -> lifetime_clause_editor
    | Some_function_name -> inside_clause_editor
    | No_function_name -> not_inside_clause_editor
    | On_heap -> on_heap_clause_editor
    | Not_on_heap -> not_on_heap_clause_editor)
  |> Editor.map_view ~f:(with_conjunction "and")
;;

let hidden_function_clause_editor : Filter_spec.Clause.t Editor.t =
  function_clause_editor
    ~clause_ctor:(fun pred -> Hide_function pred)
    ~text_before:"whose name"
  |> Editor.map_view ~f:(with_conjunction "or")
;;

let component ~max_time ~start_time ~time_view =
  let open Bonsai.Let_syntax in
  let open Vdom in
  let time_parameters =
    let%map max_time and start_time and time_view in
    { Time_parameters.max_time; start_time; time_view }
  in
  let add_button_attr = Attr.class_ "flat-button" in
  let remove_button_attr = add_button_attr in
  let%sub clause_list_editor =
    List_editor.component
      ~attr:(Attr.class_ "clauses")
      ~add_button_attr
      ~remove_button_attr
      ~add_item_text:"Add filter"
      (toplevel_clause_editor ~time_parameters)
  in
  let%sub hidden_function_clause_list_editor =
    List_editor.component
      ~attr:(Attr.class_ "clauses")
      ~add_button_attr
      ~remove_button_attr
      ~add_item_text:"Add filter"
      (hidden_function_clause_editor ~time_parameters)
  in
  return
    (let%map { And_view.value = clauses; view = clauses_view } = clause_list_editor
     and { And_view.value = hidden_function_clauses; view = hidden_function_clauses_view }
       =
       hidden_function_clause_list_editor
     in
     let value = clauses @ hidden_function_clauses in
     let view =
       Node.div
         [ Node.p [ Node.text "Only show allocations:" ]
         ; clauses_view
         ; Node.p [ Node.text "Hide functions:" ]
         ; hidden_function_clauses_view
         ]
     in
     { And_view.value; view })
;;
