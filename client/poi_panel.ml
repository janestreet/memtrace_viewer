open! Core
open Bonsai_web
open Memtrace_viewer_common

module Orientations = struct
  type t =
    | Both
    | Only of Data.Orientation.t
end

module State = struct
  type t =
    | Direction of Data.Orientation.t
    | Hot_fragment of Data.Backtrace.t
  [@@deriving sexp, equal]

  let default = Direction Callees

  let poi ~trie = function
    | Direction Callees -> Data.Fragment_trie.toplevel_fragment trie
    | Direction Callers -> Data.Fragment_trie.allocation_site_fragment trie
    | Hot_fragment backtrace ->
      (match Data.Fragment_trie.find trie backtrace with
       | Some fragment -> fragment
       | None -> Data.Fragment_trie.empty_fragment trie)
  ;;

  let orientations = function
    | Direction orient -> Orientations.Only orient
    | Hot_fragment _ -> Orientations.Both
  ;;
end


module Direction_tab = struct
  module Direction_button = struct
    type t =
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

  let component ~state ~set_state =
    let open Bonsai.Let_syntax in
    let direction =
      let%map state = state in
      match state with
      | State.Direction Callees -> Some Direction_button.Explore_upwards_from_main
      | State.Direction Callers ->
        Some Direction_button.Explore_downwards_from_allocations
      | Hot_fragment _ -> None
    in
    let set_direction =
      let%map set_state = set_state in
      function
      | Direction_button.Explore_downwards_from_allocations ->
        set_state (State.Direction Callers)
      | Explore_upwards_from_main -> set_state (State.Direction Callees)
    in
    let%sub direction_list =
      Radio_list.component
        (module Direction_button)
        ~name:(Bonsai.Value.return "direction")
        ~value:direction
        ~set_value:set_direction
    in
    return
      (let%map direction_list = direction_list in
       let view =
         let open Vdom in
         Node.div [ Node.p [ Node.text "Explore:" ]; direction_list ]
       in
       view, ())
  ;;
end

module Table_tab = struct
  module Which_table = struct
    type t =
      | Call_sites of { hot_call_sites : Data.Fragment.t list Bonsai.Value.t }
      | Hot_paths of { hot_paths : Data.Fragment.t list Bonsai.Value.t }
  end

  let row_of fragment =
    let display = Data.Fragment.backtrace fragment in
    let allocations = Data.Entry.allocations (Data.Fragment.entry fragment) in
    let fragment = Data.Fragment.representative fragment in
    { Location_table.Row.fragment; display; allocations }
  ;;

  let rows_of fragments = List.map ~f:row_of fragments

  let component ~which_table ~total_allocations ~set_state =
    let open Bonsai.Let_syntax in
    let rows =
      match which_table with
      | Which_table.Call_sites { hot_call_sites } -> rows_of <$> hot_call_sites
      | Hot_paths { hot_paths } -> rows_of <$> hot_paths
    in
    let presorted = Bonsai.Value.return true in
    let%sub ({ Location_table.selection; _ } as table) =
      Location_table.component ~total_allocations ~rows ~presorted
    in
    let selection_backtrace =
      let%map selection = selection in
      Option.map ~f:fst selection
    in
    let callback =
      let%map set_state = set_state in
      function
      | None -> Vdom.Effect.Ignore
      | Some selection_backtrace -> set_state (State.Hot_fragment selection_backtrace)
    in
    let%sub () =
      Bonsai.Edge.on_change
        [%here]
        (module Util.Option_model (Data.Backtrace))
        selection_backtrace
        ~callback
    in
    return
      (let%map { Location_table.view
               ; key_handler
               ; selection = _
               ; set_selection = _
               ; move_selection = _
               }
        =
        table
       in
       (* Only send keyboard events to this table if it's focused (in the browser sense)
       *)
       let { Keyboard_scope.view; key_help = _ } =
         Keyboard_scope.wrap ~view ~key_handler
       in
       view, ())
  ;;
end

module Tab = struct
  module Input = struct
    type t =
      { total_allocations : Byte_units.t Bonsai.Value.t
      ; state : State.t Bonsai.Value.t
      ; set_state : (State.t -> unit Ui_effect.t) Bonsai.Value.t
      ; hot_call_sites : Data.Fragment.t list Bonsai.Value.t
      ; hot_paths : Data.Fragment.t list Bonsai.Value.t
      }
  end

  module Output = Unit

  type t =
    | Directions
    | Call_sites
    | Hot_paths
  [@@deriving sexp, compare, enumerate, equal]

  let name = function
    | Directions -> "Endpoints"
    | Call_sites -> "Call sites"
    | Hot_paths -> "Hot Paths"
  ;;

  let initial = Directions
  let enabled ~input:_ = Bonsai.Value.return (fun (_ : t) -> true)

  let component t ~input ~select_tab:_ =
    let { Input.total_allocations; state; set_state; hot_call_sites; hot_paths } =
      input
    in
    match t with
    | Directions -> Direction_tab.component ~state ~set_state
    | Call_sites ->
      let which_table = Table_tab.Which_table.Call_sites { hot_call_sites } in
      Table_tab.component ~which_table ~total_allocations ~set_state
    | Hot_paths ->
      let which_table = Table_tab.Which_table.Hot_paths { hot_paths } in
      Table_tab.component ~which_table ~total_allocations ~set_state
  ;;
end

type t =
  { view : Vdom.Node.t
  ; orientations : Orientations.t
  ; poi : Data.Fragment.t
  }

let component ~trie ~hot_paths ~hot_call_sites ~set_focus =
  let open Bonsai.Let_syntax in
  let%sub state, set_state =
    Bonsai.state [%here] (module State) ~default_model:State.default
  in
  let%sub total_allocations =
    return
      (let%map trie = trie in
       Data.Fragment_trie.total_allocations trie)
  in
  let%sub set_state =
    return
      (let%map set_state = set_state
       and trie = trie
       and set_focus = set_focus in
       fun new_state ->
         let poi = State.poi ~trie new_state in
         Vdom.Effect.Many [ set_focus poi; set_state new_state ])
  in
  let input =
    { Tab.Input.total_allocations; hot_paths; hot_call_sites; state; set_state }
  in
  let%sub tab_panel = Tab_panel.component ~input (module Tab) in
  return
    (let%map { view; _ } = tab_panel
     and trie = trie
     and state = state in
     let view =
       let open Vdom in
       Node.div
         ~attr:(Attr.id "poi-panel-container")
         [ Panel.panel view ~id:"poi-panel" ~title:"Points of Interest" ]
     in
     let orientations = State.orientations state in
     let poi = State.poi ~trie state in
     { view; orientations; poi })
;;
