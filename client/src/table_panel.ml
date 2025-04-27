open! Core
open! Async_kernel
open! Bonsai_web_proc
open Vdom_keyboard
open Memtrace_viewer_common

module State = struct
  type t = { expanded_backtraces : Data.Backtrace.Set.t } [@@deriving sexp, equal]

  let empty = { expanded_backtraces = Data.Backtrace.Set.empty }

  let expand { expanded_backtraces } fragment =
    { expanded_backtraces = Set.add expanded_backtraces (Data.Fragment.backtrace fragment)
    }
  ;;

  let collapse { expanded_backtraces } fragment =
    { expanded_backtraces =
        Set.remove expanded_backtraces (Data.Fragment.backtrace fragment)
    }
  ;;

  let is_expanded { expanded_backtraces } fragment =
    Set.mem expanded_backtraces (Data.Fragment.backtrace fragment)
  ;;
end

module Action = struct
  type t =
    | Expand of Data.Fragment.Debug.t (* provides sexp_of *)
    | Collapse of Data.Fragment.Debug.t
    | Reset
  [@@deriving sexp_of]
end

let state_machine =
  let apply_action (_ : _ Bonsai.Apply_action_context.t) state = function
    | Action.Expand fragment -> State.expand state fragment
    | Collapse fragment -> State.collapse state fragment
    | Reset -> State.empty
  in
  let default_model = State.empty in
  Bonsai.state_machine
    ()
    ~equal:[%equal: State.t]
    ~sexp_of_action:[%sexp_of: Action.t]
    ~apply_action
    ~default_model
;;

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  ; selection : Data.Fragment.t option
  ; reset_selection : unit -> unit Ui_effect.t
  }

let transform_input ~orient ~focus ~state =
  let rec add_node rows (loc, fragment) =
    let expanded = State.is_expanded state fragment in
    let entry = Data.Fragment.entry fragment in
    let hidden = expanded && not (Data.Entry.is_heavy entry) in
    let rows =
      if hidden
      then rows
      else (
        let display = [ loc ] in
        let allocations =
          if expanded
          then Data.Entry.direct_allocations entry
          else Data.Entry.allocations entry
        in
        let row = { Location_table.Row.fragment; display; allocations } in
        row :: rows)
    in
    if expanded
    then add_nodes rows (Data.Fragment.one_frame_extensions ~orient fragment)
    else rows
  and add_nodes rows alist = List.fold ~init:rows ~f:add_node alist in
  let roots = Data.Fragment.one_frame_extensions ~orient focus in
  add_nodes [] roots
;;

let and_prevent_default event = Vdom.Effect.Many [ Vdom.Effect.Prevent_default; event ]

let unroll_action ~orient inject_action selection =
  let keystroke = Vdom_keyboard.Keystroke.create' ArrowRight in
  match selection with
  | Some selection when Data.Fragment.has_extensions ~orient selection ->
    let keys = [ keystroke ] in
    let description = "Expand selected item" in
    let group = None in
    let handler _ = inject_action (Action.Expand selection) |> and_prevent_default in
    Keyboard_event_handler.Action.Command { keys; description; group; handler }
  | _ -> Disabled_key keystroke
;;

let reroll_action ~orient inject_action selection state =
  let keystroke = Vdom_keyboard.Keystroke.create' ArrowLeft in
  let disabled = Keyboard_event_handler.Action.Disabled_key keystroke in
  match selection with
  | None -> disabled
  | Some selection ->
    (match Data.Fragment.retract ~orient selection with
     | None -> disabled
     | Some parent ->
       let has_expanded_parent = State.is_expanded state parent in
       if has_expanded_parent
       then (
         let keys = [ keystroke ] in
         let description = "Collapse selected item" in
         let group = None in
         let handler _ = inject_action (Action.Collapse parent) |> and_prevent_default in
         Keyboard_event_handler.Action.Command { keys; description; group; handler })
       else disabled)
;;

let component ~(data : Data.t Bonsai.Value.t) ~orient ~focus ~set_focus =
  let open Bonsai.Let_syntax in
  let%sub state_machine in
  let%sub table_input =
    let%arr orient
    and focus
    and state, _ = state_machine in
    transform_input ~focus ~state ~orient
  in
  let%sub table =
    let total_allocations =
      let%map data in
      Data.Fragment_trie.total_allocations data.trie
    in
    let%sub Data.{ call_sites; _ } = Bonsai.read data in
    (* For consistency with the flame graph, don't set the focus on single click. A double
       click will still set the focus (as with the flame graph). *)
    let on_click_row = Bonsai.Value.return (fun _ -> Vdom.Effect.Ignore) in
    Location_table.component
      ~total_allocations
      ~call_sites
      ~rows:table_input
      ~presorted:(Bonsai.Value.return false)
      ~focus
      ~set_focus
      ~on_click_row
  in
  return
    (let%map state, inject = state_machine
     and table
     and orient in
     let selection = Option.map ~f:snd table.selection in
     let key_handler =
       Keyboard_event_handler.merge
         ~on_dup:`Throw
         table.Location_table.key_handler
         (Keyboard_event_handler.of_action_list_exn
            [ unroll_action ~orient inject selection
            ; reroll_action ~orient inject selection state
            ])
     in
     let view = Vdom.Node.div ~attrs:[ Vdom.Attr.id "table-panel" ] [ table.view ] in
     let reset_selection () =
       match table.selection with
       | Some _ ->
         Vdom.Effect.Many
           [ table.set_selection None; table.move_selection `Next; inject Reset ]
       | None -> inject Reset
     in
     { view; key_handler; selection; reset_selection })
;;
