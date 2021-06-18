open! Core
open Bonsai_web
open Memtrace_viewer_common
module Default_selection = Flame_graph_panel.Default_selection

module State = struct
  type t = { focus_backtrace : Data.Backtrace.t } [@@deriving sexp, equal]

  let default =
    let focus_backtrace = [ Data.Location.toplevel ] in
    { focus_backtrace }
  ;;

  let focus ~trie t =
    match Data.Fragment_trie.find trie t.focus_backtrace with
    | None -> Data.Fragment_trie.empty_fragment trie
    | Some focus -> focus
  ;;

  module Action = struct
    type t =
      { new_focus : Data.Fragment.t
      ; default_selection : Default_selection.t
      }

    let select ~inject new_focus default_selection () =
      inject { new_focus; default_selection }
    ;;

    let set ~inject default_selection new_focus = inject { new_focus; default_selection }
  end

  let apply_action
        ~schedule_event
        ~reset_selection
        (_ : t)
        { Action.new_focus; default_selection }
    =
    let focus_backtrace = Data.Fragment.backtrace new_focus in
    schedule_event (reset_selection new_focus default_selection);
    { focus_backtrace }
  ;;
end

module Selection = struct
  module Flame_graph = struct
    type t =
      | Flame of
          { fragment : Data.Fragment.t
          ; extend_focus_to : unit -> Vdom.Event.t
          }
      | Icicle of
          { fragment : Data.Fragment.t
          ; extend_focus_to : unit -> Vdom.Event.t
          }
      | Focus of
          { location : Data.Location.t
          ; retract_callees_from_focus : (unit -> Vdom.Event.t) option
          ; retract_callers_from_focus : (unit -> Vdom.Event.t) option
          }

    let location selection =
      match selection with
      | Flame { fragment; _ } -> Data.Fragment.first ~orient:Callees fragment
      | Icicle { fragment; _ } -> Data.Fragment.first ~orient:Callers fragment
      | Focus { location; _ } -> location
    ;;
  end

  module Table = struct
    type t =
      { fragment : Data.Fragment.t
      ; extend_focus_to : (unit -> Vdom.Event.t) option
      }

    let location ~orient { fragment; _ } = Data.Fragment.first ~orient fragment
  end

  type t =
    | Flame_graph of { selection : Flame_graph.t option }
    | Table of
        { orient : Data.Orientation.t
        ; selection : Table.t option
        ; retract_from_focus : (unit -> Vdom.Event.t) option
        }

  let location = function
    | Flame_graph { selection = Some selection } -> Some (Flame_graph.location selection)
    | Table { selection = Some selection; orient; _ } ->
      Some (Table.location ~orient selection)
    | Flame_graph { selection = None } | Table { selection = None; _ } -> None
  ;;
end

module Tab = struct
  module Input = struct
    type t =
      { data : Data.t Bonsai.Value.t
      ; focus : Data.Fragment.t Bonsai.Value.t
      ; inject : (State.Action.t -> Ui_event.t) Bonsai.Value.t
      }
  end

  type t =
    | Flame_graph
    | Callee_table
    | Caller_table
  [@@deriving sexp, compare, enumerate, equal]

  module Output = struct
    type t =
      { key_handler : Vdom_keyboard.Keyboard_event_handler.t
      ; selection : Selection.t
      ; reset_selection : Data.Fragment.t -> Default_selection.t -> Ui_event.t
      }
  end

  let table_panel ~data ~focus ~orient ~inject ~select_tab:_
    : (Vdom.Node.t * Output.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub table_panel =
      Table_panel.component ~data ~focus ~orient:(Bonsai.Value.return orient)
    in
    return
      (let%map { Table_panel.selection; key_handler; view; reset_selection } = table_panel
       and focus = focus
       and inject = inject in
       let selection =
         match selection with
         | None -> None
         | Some fragment ->
           let extend_focus_to =
             if Data.Fragment.has_extensions ~orient fragment
             then Some (State.Action.select ~inject fragment No_selection)
             else None
           in
           Some { Selection.Table.fragment; extend_focus_to }
       in
       let retract_from_focus =
         match Data.Fragment.retract ~orient focus with
         | None -> None
         | Some fragment ->
           if Data.Fragment.is_empty fragment
           then None
           else Some (State.Action.select ~inject fragment No_selection)
       in
       let selection = Selection.Table { selection; orient; retract_from_focus } in
       let reset_selection _ _ = reset_selection () in
       view, { Output.key_handler; selection; reset_selection })
  ;;

  let flame_graph_selection
        ~focus
        ~inject
        (selection : Flame_graph_panel.Selection.t option)
    : Selection.Flame_graph.t option
    =
    match selection with
    | None -> None
    | Some (Flame { fragment }) ->
      let extend_focus_to = State.Action.select ~inject fragment First_callee in
      Some (Flame { fragment; extend_focus_to })
    | Some (Icicle { fragment }) ->
      let extend_focus_to = State.Action.select ~inject fragment First_caller in
      Some (Icicle { fragment; extend_focus_to })
    | Some (Focus { callees_fragment; callers_fragment }) ->
      let location = Data.Fragment.first ~orient:Callers callers_fragment in
      let retract_callees_from_focus =
        if Data.Fragment.same callees_fragment focus
        then None
        else Some (State.Action.select ~inject callees_fragment First_callee)
      in
      let retract_callers_from_focus =
        if Data.Fragment.same callers_fragment focus
        then None
        else Some (State.Action.select ~inject callers_fragment First_caller)
      in
      Some (Focus { location; retract_callees_from_focus; retract_callers_from_focus })
  ;;

  let flame_graph_panel ~data ~focus ~inject ~select_tab:_
    : (Vdom.Node.t * Output.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let trie =
      let%map data : Data.t Bonsai.Value.t = data in
      data.trie
    in
    let activate =
      let%map inject = inject in
      function
      | Flame_graph_panel.Selection.Flame { fragment } ->
        let new_focus = fragment in
        let default_selection = Default_selection.First_callee in
        inject { State.Action.new_focus; default_selection }
      | Flame_graph_panel.Selection.Icicle { fragment } ->
        let new_focus = fragment in
        let default_selection = Default_selection.First_caller in
        inject { State.Action.new_focus; default_selection }
      | Flame_graph_panel.Selection.Focus _ -> Vdom.Event.Ignore
    in
    let%sub flame_graph_panel = Flame_graph_panel.component ~trie ~focus ~activate in
    return
      (let%map { Flame_graph_panel.view; key_handler; selection; reset_selection } =
         flame_graph_panel
       and focus = focus
       and inject = inject in
       let selection = flame_graph_selection ~focus ~inject selection in
       let selection = Selection.Flame_graph { selection } in
       view, { Output.key_handler; selection; reset_selection })
  ;;

  let name = function
    | Callee_table -> "Callee Table"
    | Caller_table -> "Caller Table"
    | Flame_graph -> "Flame Graph"
  ;;

  let component t ~input:{ Input.data; focus; inject } =
    match t with
    | Callee_table -> table_panel ~data ~focus ~orient:Callees ~inject
    | Caller_table -> table_panel ~data ~focus ~orient:Callers ~inject
    | Flame_graph -> flame_graph_panel ~data ~focus ~inject
  ;;

  let initial = Flame_graph

  let enabled ~input =
    let open Bonsai.Let_syntax in
    let%map focus = input.Input.focus in
    let has_callers = Data.Fragment.has_extensions ~orient:Callers focus in
    let has_callees = Data.Fragment.has_extensions ~orient:Callees focus in
    fun tab ->
      match tab, has_callers, has_callees with
      | Callee_table, _, false -> false
      | Caller_table, false, _ -> false
      | _ -> true
  ;;
end

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : Selection.t
  ; focus : Data.Fragment.t
  ; set_focus : Data.Fragment.t -> Vdom.Event.t
  }

let component ~(data : Data.t Bonsai.Value.t) : t Bonsai.Computation.t =
  let open Bonsai.Let_syntax in
  let apply_action ~inject:_ ~schedule_event (_, _, tab_panel) state action =
    let { Tab_panel.output = { Tab.Output.reset_selection; _ }; _ } = tab_panel in
    State.apply_action ~schedule_event ~reset_selection state action
  in
  let%sub result =
    Bonsai.wrap
      (module State)
      ~default_model:State.default
      ~apply_action
      ~f:(fun state inject ->
        let%sub focus =
          return
            (let%map state = state
             and { trie; _ } = data in
             State.focus ~trie state)
        in
        let input : Tab.Input.t = { data; focus; inject } in
        let%sub tab_panel = Tab_panel.component ~input (module Tab) in
        return
          (let%map focus = focus
           and inject = inject
           and tab_panel = tab_panel in
           focus, inject, tab_panel))
  in
  return
    (let%map focus, inject, tab_panel = result in
     let { Tab_panel.view; output; _ } = tab_panel in
     let { Tab.Output.key_handler; selection; _ } = output in
     let set_focus = State.Action.set ~inject No_selection in
     let view =
       Vdom.Node.div
         ~attr:(Vdom.Attr.id "main-panel-container")
         [ Panel.panel ~id:"main-panel" view ]
     in
     { view; key_handler; selection; focus; set_focus })
;;
