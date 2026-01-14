open! Core
open Bonsai_web_proc
open Memtrace_viewer_common
module Default_selection = App_state.Default_selection

module Selection = struct
  module Flame_graph = struct
    type t =
      | Flame of
          { fragment : Data.Fragment.t
          ; extend_focus_to : unit Vdom.Effect.t
          }
      | Icicle of
          { fragment : Data.Fragment.t
          ; extend_focus_to : unit Vdom.Effect.t
          }
      | Focus of
          { location : Data.Location.t
          ; retract_callees_from_focus : unit Vdom.Effect.t option
          ; retract_callers_from_focus : unit Vdom.Effect.t option
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
      ; extend_focus_to : unit Vdom.Effect.t option
      }

    let location ~orient { fragment; _ } = Data.Fragment.first ~orient fragment
  end

  type t =
    | Flame_graph of { selection : Flame_graph.t option }
    | Table of
        { orient : Orientation.t
        ; selection : Table.t option
        ; retract_from_focus : unit Vdom.Effect.t option
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
      ; set_focus :
          (Data.Fragment.t -> default_selection:Default_selection.t -> unit Ui_effect.t)
            Bonsai.Value.t
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
      ; reset_selection :
          Data.Fragment.t -> default_selection:Default_selection.t -> unit Ui_effect.t
      ; scroll_focus_into_view : unit Ui_effect.t
      }
  end

  let table_panel ~data ~focus ~orient ~set_focus ~select_tab:_
    : (Vdom.Node.t * Output.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let set_focus =
      let%map set_focus in
      fun fragment -> set_focus fragment ~default_selection:Default_selection.No_selection
    in
    let%sub table_panel =
      Table_panel.component ~data ~focus ~set_focus ~orient:(Bonsai.Value.return orient)
    in
    return
      (let%map { Table_panel.selection; key_handler; view; reset_selection } = table_panel
       and focus
       and set_focus in
       let selection =
         match selection with
         | None -> None
         | Some fragment ->
           let extend_focus_to =
             if Data.Fragment.has_extensions ~orient fragment
             then Some (set_focus fragment)
             else None
           in
           Some { Selection.Table.fragment; extend_focus_to }
       in
       let retract_from_focus =
         match Data.Fragment.retract ~orient focus with
         | None -> None
         | Some fragment ->
           if Data.Fragment.is_empty fragment then None else Some (set_focus fragment)
       in
       let selection = Selection.Table { selection; orient; retract_from_focus } in
       let reset_selection _ ~default_selection:_ = reset_selection () in
       let scroll_focus_into_view = Effect.Ignore in
       view, { Output.key_handler; selection; reset_selection; scroll_focus_into_view })
  ;;

  let flame_graph_selection
    ~focus
    ~set_focus
    (selection : Flame_graph_panel.Selection.t option)
    : Selection.Flame_graph.t option
    =
    match selection with
    | None -> None
    | Some (Flame { fragment }) ->
      let extend_focus_to =
        set_focus fragment ~default_selection:Default_selection.First_callee
      in
      Some (Flame { fragment; extend_focus_to })
    | Some (Icicle { fragment }) ->
      let extend_focus_to = set_focus fragment ~default_selection:First_caller in
      Some (Icicle { fragment; extend_focus_to })
    | Some (Focus { callees_fragment; callers_fragment }) ->
      let location = Data.Fragment.first ~orient:Callers callers_fragment in
      let retract_callees_from_focus =
        if Data.Fragment.same callees_fragment focus
        then None
        else Some (set_focus callees_fragment ~default_selection:First_callee)
      in
      let retract_callers_from_focus =
        if Data.Fragment.same callers_fragment focus
        then None
        else Some (set_focus callers_fragment ~default_selection:First_caller)
      in
      Some (Focus { location; retract_callees_from_focus; retract_callers_from_focus })
  ;;

  let flame_graph_panel ~data ~focus ~set_focus ~select_tab:_
    : (Vdom.Node.t * Output.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub Data.{ trie; call_sites; _ } = Bonsai.read data in
    let activate =
      let%map set_focus in
      function
      | Flame_graph_panel.Selection.Flame { fragment } ->
        let new_focus = fragment in
        let default_selection = Default_selection.First_callee in
        set_focus new_focus ~default_selection
      | Flame_graph_panel.Selection.Icicle { fragment } ->
        let new_focus = fragment in
        let default_selection = Default_selection.First_caller in
        set_focus new_focus ~default_selection
      | Flame_graph_panel.Selection.Focus _ -> Vdom.Effect.Ignore
    in
    let%sub flame_graph_panel =
      Flame_graph_panel.component ~trie ~call_sites ~focus ~set_focus ~activate
    in
    return
      (let%map { Flame_graph_panel.view
               ; key_handler
               ; selection
               ; reset_selection
               ; scroll_focus_into_view
               }
         =
         flame_graph_panel
       and focus
       and set_focus in
       let selection = flame_graph_selection ~focus ~set_focus selection in
       let selection = Selection.Flame_graph { selection } in
       view, { Output.key_handler; selection; reset_selection; scroll_focus_into_view })
  ;;

  let name = function
    | Callee_table -> "Callee Table"
    | Caller_table -> "Caller Table"
    | Flame_graph -> "Flame Graph"
  ;;

  let component t ~input:{ Input.data; focus; set_focus } =
    match t with
    | Callee_table -> table_panel ~data ~focus ~orient:Callees ~set_focus
    | Caller_table -> table_panel ~data ~focus ~orient:Callers ~set_focus
    | Flame_graph -> flame_graph_panel ~data ~focus ~set_focus
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
  ; set_focus : Data.Fragment.t -> unit Vdom.Effect.t
  }

let component ~(data : Data.t Bonsai.Value.t) ~(app_state : App_state.t Bonsai.Value.t)
  : t Bonsai.Computation.t
  =
  let open Bonsai.Let_syntax in
  (* We need to resolve a circularity: The input to the tab panel includes the effect to
     run in order to set the focus, but we want that effect to include running the
     [reset_selection] effect that's _returned_ by the tab panel. We tie the knot using
     [With_interpreter]. *)
  let module Action = struct
    type t =
      | Set_focus of
          { new_focus : Data.Fragment.t
          ; default_selection : Default_selection.t
          }
  end
  in
  let interpret
    ~value:{ Tab_panel.output = { Tab.Output.reset_selection; _ }; _ }
    ~context:set_focus_in_app_state
    (Action.Set_focus { new_focus; default_selection })
    =
    set_focus_in_app_state new_focus ~default_selection ~reset_selection
  in
  let%sub { focus; set_focus = set_focus_in_app_state } = Bonsai.read app_state in
  let%sub With_interpreter.{ value = tab_panel; run_action } =
    With_interpreter.component ~interpret ~input:(fun ~run_action ->
      let set_focus =
        let%map run_action in
        fun new_focus ~default_selection ->
          run_action (Set_focus { new_focus; default_selection })
      in
      let input : Tab.Input.t = { data; focus; set_focus } in
      let%sub tab_panel = Tab_panel.component ~input (module Tab) in
      let%arr set_focus_in_app_state and tab_panel in
      With_interpreter.Input.{ value = tab_panel; context = set_focus_in_app_state })
  in
  let%sub { Tab_panel.view = panel_body; output = { scroll_focus_into_view; _ }; _ } =
    Bonsai.read tab_panel
  in
  let%sub panel = Panel.panel ~id:"main-panel" ~collapsible:No panel_body in
  let%sub set_focus =
    (* Careful of the dependencies here: We're accessing (mapping over) _only_ the
       individual values [run_action] and [scroll_focus_into_view], which should rarely
       change. A previous version accessed the whole of [tab_panel] instead and then
       projected out [scroll_focus_into_view], making this [Value.t] sensitive to any
       change at all in the tab panel. Since here we're producing a fresh closure with
       every re-evaluation, the ensuing cascade touched every row of the table whenever,
       say, the selection changed.
    *)
    let%arr run_action and scroll_focus_into_view in
    fun new_focus ->
      Effect.Many
        [ run_action (Set_focus { new_focus; default_selection = No_selection })
        ; scroll_focus_into_view
        ]
  in
  return
    (let%map panel
     and set_focus
     and { Tab_panel.output; _ } = tab_panel in
     let { Tab.Output.key_handler; selection; _ } = output in
     let view = panel in
     { view; key_handler; selection; set_focus })
;;
