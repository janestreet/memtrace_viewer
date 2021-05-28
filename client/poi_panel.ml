open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common


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

  let component ~trie ~poi ~set_poi =
    let open Bonsai.Let_syntax in
    let direction =
      let%map poi = poi in
      match Data.Fragment.backtrace poi with
      | [ loc ] when Data.Location.is_allocation_site loc ->
        Some Direction_button.Explore_downwards_from_allocations
      | [ loc ] when Data.Location.is_toplevel loc -> Some Explore_upwards_from_main
      | _ -> None
    in
    let set_direction =
      let%map trie = trie
      and set_poi = set_poi in
      function
      | Direction_button.Explore_downwards_from_allocations ->
        set_poi (Data.Fragment_trie.allocation_site_fragment trie)
      | Explore_upwards_from_main -> set_poi (Data.Fragment_trie.toplevel_fragment trie)
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
         Node.div [] [ Node.p [] [ Node.text "Explore:" ]; direction_list ]
       in
       view, ())
  ;;
end

let all_representatives trie =
  let rec descend acc (_, fragment) =
    let acc =
      if Data.Fragment.same fragment (Data.Fragment.representative fragment)
      then fragment :: acc
      else acc
    in
    List.fold
      ~f:descend
      ~init:acc
      (Data.Fragment.one_frame_extensions fragment ~orient:Callees)
  in
  descend [] (Data.Location.dummy, Data.Fragment_trie.empty_fragment trie)
;;

module Table_tab = struct
  module Which_table = struct
    type t =
      | Locations
      | Hot_paths
  end

  let row_of fragment =
    let backtrace =
      Data.Fragment.backtrace fragment |> Data.Fragment.Extension.of_callees
    in
    let allocations = Data.Entry.allocations (Data.Fragment.entry fragment) in
    { Location_table.Row.backtrace; allocations }
  ;;

  let is_trivial fragment =
    match Data.Fragment.retract ~orient:Callees fragment with
    | None ->
      (* empty fragment *)
      true
    | Some retraction ->
      if Data.Fragment.is_empty retraction
      then (
        (* singleton fragment *)
        match Data.Fragment.backtrace fragment with
        | [ loc ] -> Data.Location.is_special loc
        | _ -> assert false)
      else false
  ;;

  let locations_rows trie =
    List.filter_map (all_representatives trie) ~f:(fun fragment ->
      if is_trivial fragment then None else Some (row_of fragment))
  ;;

  let hot_paths_rows hot_paths = List.map ~f:row_of hot_paths

  let component ~which_table ~trie ~hot_paths ~poi ~set_poi =
    let open Bonsai.Let_syntax in
    let total_allocations = Data.Fragment_trie.total_allocations <$> trie in
    let rows =
      match which_table with
      | Which_table.Locations -> locations_rows <$> trie
      | Hot_paths -> hot_paths_rows <$> hot_paths
    in
    let presorted =
      Bonsai.Value.return
        (match which_table with
         | Locations -> false
         | Hot_paths -> true)
    in
    let only_show_last_frame =
      Bonsai.Value.return
        (match which_table with
         | Locations -> true
         | Hot_paths -> false)
    in
    let%sub ({ Location_table.focus; _ } as table) =
      Location_table.component ~total_allocations ~rows ~presorted ~only_show_last_frame
    in
    let callees_of_fragment_extension
          ({ Data.Fragment.Extension.callees; callers } as backtrace)
      =
      if List.is_empty (Data.Backtrace.Reversed.elements callers)
      then callees
      else
        raise_s [%message "expected no callers" (backtrace : Data.Fragment.Extension.t)]
    in
    let poi_backtrace = Data.Fragment.backtrace <$> poi in
    let%sub set_poi_on_change =
      Bonsai.Edge.on_change
        [%here]
        (module Util.Option_model (Data.Fragment.Extension))
        focus
        ~callback:
          (let%mapn trie = trie
           and poi_backtrace = poi_backtrace
           and set_poi = set_poi in
           fun focus ->
             match focus with
             | Some focus
               when not
                      (Data.Fragment.Extension.equal
                         focus
                         (poi_backtrace |> Data.Fragment.Extension.of_callees)) ->
               (match
                  Data.Fragment_trie.find trie (focus |> callees_of_fragment_extension)
                with
                | Some focus_fragment -> set_poi focus_fragment
                | None -> Vdom.Event.Ignore)
             | _ -> Vdom.Event.Ignore)
    in
    return
      (let%map { Location_table.view
               ; key_handler
               ; focus = _
               ; set_focus = _
               ; move_focus = _
               }
        =
        table
       and () = set_poi_on_change in
       (* Only send keyboard events to this table if it's focused (in the browser sense)
       *)
       let { Keyboard_scope.view; key_help = _ } =
         Keyboard_scope.wrap ~view ~key_handler
       in
       view, ())
  ;;
end

module Tab = struct
  module Extra = Unit

  type t =
    | Directions
    | Hot_paths
    | Locations
  [@@deriving sexp, compare, enumerate, equal]

  let name = function
    | Directions -> "Endpoints"
    | Locations -> "Locations"
    | Hot_paths -> "Hot Paths"
  ;;

  let initial = Directions
  let enabled = Bonsai.const (fun (_ : t) -> true)
end

let component ~trie ~hot_paths ~poi ~set_poi =
  let open Bonsai.Let_syntax in
  let module Tab = struct
    include Tab

    let component t ~select_tab:_ =
      match t with
      | Directions -> Direction_tab.component ~trie ~poi ~set_poi
      | Locations ->
        Table_tab.component ~which_table:Locations ~trie ~hot_paths ~poi ~set_poi
      | Hot_paths ->
        Table_tab.component ~which_table:Hot_paths ~trie ~hot_paths ~poi ~set_poi
    ;;
  end
  in
  let%sub tab_panel = Tab_panel.component (module Tab) in
  return
    (let%map tab_panel = tab_panel in
     let open Vdom in
     Node.div
       [ Attr.id "poi-panel-container" ]
       [ Panel.panel tab_panel.view ~id:"poi-panel" ~title:"Points of Interest" ])
;;
