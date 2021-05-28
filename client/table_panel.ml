open! Core_kernel
open! Async_kernel
open! Bonsai_web
open Vdom_keyboard
open Memtrace_viewer_common

module State = struct
  type t = { expanded_backtraces : Data.Backtrace.Set.t } [@@deriving sexp, equal, fields]

  let empty = { expanded_backtraces = Data.Backtrace.Set.empty }

  let expand { expanded_backtraces } fragment =
    { expanded_backtraces =
        Data.Backtrace.Set.add expanded_backtraces (Data.Fragment.backtrace fragment)
    }
  ;;

  let collapse { expanded_backtraces } fragment =
    { expanded_backtraces =
        Data.Backtrace.Set.remove expanded_backtraces (Data.Fragment.backtrace fragment)
    }
  ;;

  let is_expanded { expanded_backtraces } fragment =
    Data.Backtrace.Set.mem expanded_backtraces (Data.Fragment.backtrace fragment)
  ;;
end

module Action = struct
  type t =
    | Expand of Data.Fragment.Debug.t (* provides sexp_of *)
    | Collapse of Data.Fragment.Debug.t
  [@@deriving sexp_of]
end

let state_machine =
  let apply_action ~inject:_ ~schedule_event:_ state = function
    | Action.Expand fragment -> State.expand state fragment
    | Collapse fragment -> State.collapse state fragment
  in
  let default_model = State.empty in
  Bonsai.state_machine0
    [%here]
    (module State)
    (module Action)
    ~apply_action
    ~default_model
;;

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; focus : Data.Fragment.Oriented.t option
  ; fix_focus : new_zoom:Data.Fragment.t -> Vdom.Event.t
  }

let transform_input ~(data : Data.t) ~orient ~zoom ~state =
  let mk_row ~backtrace ~exclude_children ~child_allocs fragment =
    let entry = Data.Fragment.entry fragment in
    let allocations =
      if exclude_children
      then Byte_units.(Data.Entry.allocations entry - child_allocs)
      else Data.Entry.allocations entry
    in
    { Location_table.Row.backtrace; allocations }
  in
  let rec add_node ~(ext : Data.Fragment.Extension.t) rows (loc, fragment) =
    let backtrace =
      (* All this appending to the ends of things is unfortunate, but they're not going
         to get very long. *)
      match orient with
      | Data.Orientation.Callees -> { ext with callees = ext.callees @ [ loc ] }
      | Callers ->
        { ext with
          callers =
            Data.Backtrace.Reversed.append
              ext.callers
              (Data.Backtrace.Reversed.of_reversed_list [ loc ])
        }
    in
    let expanded = State.is_expanded state fragment in
    let child_allocs =
      if expanded
      then
        List.fold
          (Data.Fragment.one_frame_extensions ~orient fragment)
          ~init:Byte_units.zero
          ~f:(fun child_allocs (_, child) ->
            Byte_units.(child_allocs + Data.Entry.allocations (Data.Fragment.entry child)))
      else Byte_units.zero
    in
    let too_small_after_expansion () =
      let max_error = Data.Entry.max_error (Data.Fragment.entry fragment) in
      Byte_units.(
        child_allocs + max_error < Data.Fragment_trie.significance_threshold data.trie)
    in
    let hidden = expanded && too_small_after_expansion () in
    let rows =
      if hidden
      then rows
      else (
        let exclude_children = expanded in
        let row = mk_row ~backtrace ~exclude_children ~child_allocs fragment in
        row :: rows)
    in
    if expanded
    then add_nodes ~ext rows (Data.Fragment.one_frame_extensions ~orient fragment)
    else rows
  and add_nodes ~ext rows alist = List.fold ~init:rows ~f:(add_node ~ext) alist in
  let roots = Data.Fragment.one_frame_extensions ~orient zoom in
  let ext = Data.Fragment.Extension.empty in
  add_nodes ~ext [] roots
;;

let and_prevent_default event = Vdom.Event.Many [ Vdom.Event.Prevent_default; event ]

let unroll_action ~orient inject_action focus =
  let keystroke = Vdom_keyboard.Keystroke.create' ArrowRight in
  match focus with
  | Some focus when not (List.is_empty (Data.Fragment.one_frame_extensions ~orient focus))
    ->
    let keys = [ keystroke ] in
    let description = "Expand focused item" in
    let group = None in
    let handler _ = inject_action (Action.Expand focus) |> and_prevent_default in
    Keyboard_event_handler.Action.Command { keys; description; group; handler }
  | _ -> Disabled_key keystroke
;;

let reroll_action ~orient inject_action focus state =
  let keystroke = Vdom_keyboard.Keystroke.create' ArrowLeft in
  let disabled = Keyboard_event_handler.Action.Disabled_key keystroke in
  match focus with
  | None -> disabled
  | Some focus ->
    (match Data.Fragment.retract ~orient focus with
     | None -> disabled
     | Some parent ->
       let has_expanded_parent = State.is_expanded state parent in
       if has_expanded_parent
       then (
         let keys = [ keystroke ] in
         let description = "Collapse focused item" in
         let group = None in
         let handler _ = inject_action (Action.Collapse parent) |> and_prevent_default in
         Keyboard_event_handler.Action.Command { keys; description; group; handler })
       else disabled)
;;

let transform_result
      ~orient
      ~zoom
      ~state
      ~inject_state_machine_action
      (res : Location_table.t)
  =
  let view = res.view in
  let focus_fragment =
    let%bind.Option focus_extension = res.focus in
    Data.Fragment.extend zoom focus_extension
  in
  let focus =
    let%map.Option fragment = focus_fragment in
    { Data.Fragment.Oriented.fragment; orient }
  in
  let key_handler =
    Keyboard_event_handler.merge
      ~on_dup:`Throw
      res.Location_table.key_handler
      (Keyboard_event_handler.of_action_list_exn
         [ unroll_action ~orient inject_state_machine_action focus_fragment
         ; reroll_action ~orient inject_state_machine_action focus_fragment state
         ])
  in
  let view = Vdom.Node.div [ Vdom.Attr.id "table-panel" ] [ view ] in
  let fix_focus ~new_zoom:_ =
    match focus with
    | Some _ -> Vdom.Event.Many [ res.set_focus None; res.move_focus `Next ]
    | None -> Vdom.Event.Ignore
  in
  { view; key_handler; focus; fix_focus }
;;

let component ~data ~orient ~zoom =
  let open Bonsai.Let_syntax in
  let%sub state_machine = state_machine in
  let table_input =
    let%map data = data
    and orient = orient
    and zoom = zoom
    and state, _ = state_machine in
    transform_input ~data ~zoom ~state ~orient
  in
  let%sub table =
    let total_allocations =
      let%map data = data in
      Data.Fragment_trie.total_allocations data.trie
    in
    Location_table.component
      ~total_allocations
      ~rows:table_input
      ~presorted:(Bonsai.Value.return false)
      ~only_show_last_frame:(Bonsai.Value.return false)
  in
  return
    (let%map state, inject_state_machine_action = state_machine
     and table = table
     and orient = orient
     and zoom = zoom in
     transform_result ~orient ~zoom ~state ~inject_state_machine_action table)
;;
