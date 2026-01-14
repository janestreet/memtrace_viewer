open! Core
open Bonsai_web_proc
open Vdom_keyboard
open Memtrace_viewer_common

module Graph = struct
  type t =
    { focus : Data.Fragment.t
    ; trie : Data.Fragment_trie.t
    ; call_sites : Data.Call_sites.t
    ; flame_tree : Data.Fragment.Oriented.t list
    ; icicle_tree : Data.Fragment.Oriented.t list
    ; focus_seq : Data.Fragment.Iterator.t option
    }

  module Node = struct
    type t =
      | Tree of Data.Fragment.Oriented.t
      | Focus of Data.Fragment.Iterator.t

    let location = function
      | Tree tree -> Data.Fragment.Oriented.first tree
      | Focus seq -> Data.Fragment.Iterator.location seq
    ;;

    let type_ ~graph:_ t : Flame_graph_view.Node_type.t =
      match location t with
      | Allocation_site _ -> Allocation_site
      | Function _ -> Function
      | Allocator | Toplevel | Dummy -> assert false
    ;;

    let label ~graph:_ t =
      match location t with
      | Allocation_site alloc_site -> Data.Allocation_site.short_name alloc_site
      | loc -> Data.Location.defname loc
    ;;

    let entry ~graph = function
      | Tree tree -> Data.Fragment.entry (Data.Fragment.Oriented.fragment tree)
      | Focus _ -> Data.Fragment.entry graph.focus
    ;;

    let allocations ~graph t = Data.Entry.allocations (entry ~graph t)
    let size ~graph t = allocations ~graph t |> Byte_units.bytes_float

    let details ~graph t =
      let loc = location t in
      let entry = entry ~graph t in
      let call_sites = Data.Call_sites.for_location graph.call_sites loc in
      String.concat
        [ Data.Location.full_name loc ~call_sites:(Some call_sites)
        ; ": "
        ; Data.Entry.allocations_string entry
        ; " ("
        ; Data.Entry.percentage_string entry
        ; ")"
        ]
    ;;
  end

  module Tree = struct
    type t = Data.Fragment.Oriented.t

    let node t = Node.Tree t

    let children ~graph:_ t =
      let children = Data.Fragment.Oriented.one_frame_extensions t in
      List.filter_map children ~f:(fun (loc, fragment) ->
        if Data.Location.is_special loc then None else Some fragment)
    ;;

    let parent ~graph t =
      let%bind.Option t = Data.Fragment.Oriented.retract t in
      let fragment = Data.Fragment.Oriented.fragment t in
      if Data.Fragment.same fragment graph.focus then None else Some t
    ;;

    let same (t1 : t) (t2 : t) =
      let fragment1 = Data.Fragment.Oriented.fragment t1 in
      let fragment2 = Data.Fragment.Oriented.fragment t2 in
      Data.Fragment.same fragment1 fragment2
    ;;

    let allocations t =
      let fragment = Data.Fragment.Oriented.fragment t in
      let entry = Data.Fragment.entry fragment in
      Data.Entry.allocations entry
    ;;
  end

  module Sequence = struct
    type t = Data.Fragment.Iterator.t

    let node t = Node.Focus t

    let label ~graph t =
      (* This assumes that we're labeling the entire sequence, which is to say, this is
         the initial node in the sequence. *)
      let fragment = Data.Fragment.Iterator.suffix t in
      let allocs = Data.Entry.allocations (Data.Fragment.entry fragment) in
      let total_allocs = Data.Fragment_trie.total_allocations graph.trie in
      let percentage = Byte_units.(allocs // total_allocs) *. 100. in
      [ allocs |> Byte_units.Short.to_string; sprintf "%.1f%%" percentage ]
    ;;

    let next ~graph:_ seq =
      let%bind.Option next = Data.Fragment.Iterator.next seq in
      if Data.Location.is_special (Data.Fragment.Iterator.location next)
      then None
      else Some next
    ;;

    let prev ~graph:_ seq =
      let%bind.Option prev = Data.Fragment.Iterator.prev seq in
      if Data.Location.is_special (Data.Fragment.Iterator.location prev)
      then None
      else Some prev
    ;;

    let same t1 t2 =
      let prefix1 = Data.Fragment.Iterator.prefix t1 in
      let prefix2 = Data.Fragment.Iterator.prefix t2 in
      Data.Fragment.same prefix1 prefix2
    ;;
  end

  let create ~trie ~call_sites ~focus =
    let flame_tree =
      if Data.Fragment.is_empty focus
      then []
      else (
        let orient = Orientation.Callees in
        let tree = Data.Fragment.oriented focus ~orient in
        Tree.children ~graph:() tree)
    in
    let icicle_tree =
      if Data.Fragment.is_empty focus
      then []
      else (
        let orient = Orientation.Callers in
        let tree = Data.Fragment.oriented focus ~orient in
        Tree.children ~graph:() tree)
    in
    let focus_seq =
      let%bind.Option seq = Data.Fragment.iterator_start focus in
      let rec loop seq =
        if Data.Location.is_special (Data.Fragment.Iterator.location seq)
        then (
          match Data.Fragment.Iterator.next seq with
          | None -> None
          | Some seq -> loop seq)
        else Some seq
      in
      loop seq
    in
    { trie; call_sites; focus; flame_tree; icicle_tree; focus_seq }
  ;;

  let flame_tree { flame_tree; _ } = flame_tree
  let icicle_tree { icicle_tree; _ } = icicle_tree
  let focus { focus_seq; _ } = focus_seq

  let size { flame_tree; icicle_tree; focus_seq; focus; _ } =
    let allocations =
      match focus_seq with
      | Some _ ->
        let entry = Data.Fragment.entry focus in
        Data.Entry.allocations entry
      | None ->
        let flame_allocations =
          List.fold_left
            ~init:Byte_units.zero
            ~f:(fun acc tree -> Byte_units.(acc + Tree.allocations tree))
            flame_tree
        in
        let icicle_allocations =
          List.fold_left
            ~init:Byte_units.zero
            ~f:(fun acc tree -> Byte_units.(acc + Tree.allocations tree))
            icicle_tree
        in
        Byte_units.max flame_allocations icicle_allocations
    in
    Byte_units.bytes_float allocations
  ;;
end

module Flame_graph = Flame_graph_view.Make (Graph)

module Selection = struct
  type t =
    | Flame of { fragment : Data.Fragment.t }
    | Icicle of { fragment : Data.Fragment.t }
    | Focus of
        { callers_fragment : Data.Fragment.t
        ; callees_fragment : Data.Fragment.t
        }

  let of_selector (selector : Flame_graph.Selector.t) : t =
    match selector with
    | Flame tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      Flame { fragment }
    | Icicle tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      Icicle { fragment }
    | Focus seq ->
      let callers_fragment = Data.Fragment.Iterator.suffix seq in
      let callees_fragment = Data.Fragment.Iterator.prefix seq in
      Focus { callers_fragment; callees_fragment }
  ;;
end

module Default_selection = App_state.Default_selection

module State = struct
  type t =
    | Flame of { backtrace : Data.Backtrace.t }
    | Icicle of { backtrace : Data.Backtrace.Reversed.t }
    | Focus of { trace : Data.Fragment.Iterator.Trace.t }
  [@@deriving sexp, equal]

  let to_selector ~trie : t -> Flame_graph.Selector.t option = function
    | Flame { backtrace } ->
      let%map.Option fragment = Data.Fragment_trie.find trie backtrace in
      let orient = Orientation.Callees in
      let tree = Data.Fragment.oriented fragment ~orient in
      Flame_graph.Selector.Flame tree
    | Icicle { backtrace } ->
      let%map.Option fragment = Data.Fragment_trie.find_rev trie backtrace in
      let orient = Orientation.Callers in
      let tree = Data.Fragment.oriented fragment ~orient in
      Flame_graph.Selector.Icicle tree
    | Focus { trace } ->
      let%map.Option seq = Data.Fragment_trie.find_iterator trie trace in
      Flame_graph.Selector.Focus seq
  ;;

  let of_selector : Flame_graph.Selector.t -> t = function
    | Flame tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      let backtrace = Data.Fragment.backtrace fragment in
      Flame { backtrace }
    | Icicle tree ->
      let fragment = Data.Fragment.Oriented.fragment tree in
      let backtrace = Data.Fragment.backtrace_rev fragment in
      Icicle { backtrace }
    | Focus seq ->
      let trace = Data.Fragment.Iterator.trace seq in
      Focus { trace }
  ;;

  let default ~state ~default_selection ~focus =
    match state with
    | None -> None
    | Some _ ->
      (match default_selection with
       | Default_selection.No_selection -> None
       | Default_selection.First_caller ->
         let%map.Option seq = Data.Fragment.iterator_start focus in
         let trace = Data.Fragment.Iterator.trace seq in
         Focus { trace }
       | Default_selection.First_callee ->
         let%map.Option seq = Data.Fragment.iterator_end focus in
         let trace = Data.Fragment.Iterator.trace seq in
         Focus { trace })
  ;;
end

let commands ~selection ~focus ~set_focus ~keep_selection_in_view
  : Flame_graph_view.Commands.t
  =
  let module Command = Flame_graph_view.Command in
  match selection with
  | None -> Flame_graph_view.Commands.all_disabled (* it won't be used anyway *)
  | Some selection ->
    let set_focus fragment ~default_selection =
      Effect.Many [ set_focus fragment ~default_selection; keep_selection_in_view ]
    in
    let extend_focus_to : Flame_graph_view.Command.t =
      match (selection : Selection.t) with
      | Flame { fragment } ->
        Enabled
          (set_focus fragment ~default_selection:App_state.Default_selection.First_callee)
      | Icicle { fragment } ->
        Enabled (set_focus fragment ~default_selection:First_caller)
      | Focus _ -> Disabled
    in
    let retract_callers_from_focus : Command.t =
      match selection with
      | Focus { callers_fragment; _ } when not (Data.Fragment.same callers_fragment focus)
        -> Enabled (set_focus callers_fragment ~default_selection:First_caller)
      | Focus _ | Flame _ | Icicle _ -> Disabled
    in
    let retract_callees_from_focus : Command.t =
      match selection with
      | Focus { callees_fragment; _ } when not (Data.Fragment.same callees_fragment focus)
        -> Enabled (set_focus callees_fragment ~default_selection:First_callee)
      | Focus _ | Flame _ | Icicle _ -> Disabled
    in
    { extend_focus_to; retract_callers_from_focus; retract_callees_from_focus }
;;

type t =
  { view : Vdom.Node.t
  ; key_handler : Keyboard_event_handler.t
  ; selection : Selection.t option
  ; reset_selection :
      Data.Fragment.t -> default_selection:Default_selection.t -> unit Ui_effect.t
  ; scroll_focus_into_view : unit Ui_effect.t
  }

let component ~trie ~call_sites ~focus ~set_focus ~activate =
  let open Bonsai.Let_syntax in
  let%sub state, set_state = Bonsai.state_opt () ~equal:[%equal: State.t] in
  let%sub selection =
    return
      (let%map state and trie in
       Option.bind ~f:(State.to_selector ~trie) state)
  in
  (* We need an effect that scrolls the selected node into view _after_ the next display,
     since it's not until then that it will be off screen. *)
  let%sub scroll_selection_into_view_after_display =
    After_next_display.component (Value.return Flame_graph.scroll_selection_into_view)
  in
  let%sub scroll_focus_into_view_after_display =
    After_next_display.component (Value.return Flame_graph.scroll_focus_into_view)
  in
  let%sub flame_graph =
    let set_selection =
      let%map set_state in
      fun selector -> set_state (Some (State.of_selector selector))
    in
    let activate =
      let%map activate and set_state in
      fun selector ->
        Ui_effect.Many
          [ set_state (Some (State.of_selector selector))
          ; activate (Selection.of_selector selector)
          ]
    in
    let%sub graph : Graph.t Bonsai.Computation.t =
      let%arr focus and trie and call_sites in
      Graph.create ~focus ~trie ~call_sites
    in
    let commands =
      let%map selection
      and focus
      and set_focus
      and keep_selection_in_view = scroll_selection_into_view_after_display in
      let selection = selection |> Option.map ~f:Selection.of_selector in
      commands ~selection ~focus ~set_focus ~keep_selection_in_view
    in
    Flame_graph.component
      graph
      ~selection
      ~select:set_selection
      ~navigate_to:set_selection
      ~activate
      ~commands
  in
  let selection =
    let%map selection in
    Option.map ~f:Selection.of_selector selection
  in
  return
    (let%map flame_graph
     and state
     and set_state
     and selection
     and scroll_focus_into_view = scroll_focus_into_view_after_display in
     let view = flame_graph.view in
     let key_handler = flame_graph.key_handler in
     let reset_selection focus ~default_selection =
       let new_state = State.default ~state ~default_selection ~focus in
       set_state new_state
     in
     { view; key_handler; selection; reset_selection; scroll_focus_into_view })
;;
