open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

module Table_tab = struct
  module Which_table = struct
    type t =
      | Locations of { hot_locations : Data.Fragment.t list Bonsai.Value.t }
      | Hot_paths of { hot_paths : Data.Fragment.t list Bonsai.Value.t }
  end

  let row_of fragment =
    let display = Data.Fragment.backtrace fragment in
    let allocations = Data.Entry.allocations (Data.Fragment.entry fragment) in
    { Location_table.Row.fragment; display; allocations }
  ;;

  let rows_of fragments = List.map ~f:row_of fragments

  let component
    ~which_table
    ~total_allocations
    ~call_sites
    ~toplevel_fragment
    ~allocator_fragment
    ~focus
    ~set_focus
    =
    let open Bonsai.Let_syntax in
    let%sub rows =
      let and_endpoints fragments =
        let%map fragments and toplevel_fragment and allocator_fragment in
        (* We want to put the special fragments first, so filter them out and then put
           them in manually *)
        let fragments =
          List.filter fragments ~f:(fun fragment ->
            not (Data.Fragment.is_trivial fragment))
        in
        List.concat [ [ toplevel_fragment; allocator_fragment ]; fragments ]
      in
      match which_table with
      | Which_table.Locations { hot_locations } ->
        return (rows_of <$> (hot_locations |> and_endpoints))
      | Hot_paths { hot_paths } -> return (rows_of <$> (hot_paths |> and_endpoints))
    in
    let presorted = Bonsai.Value.return true in
    let%sub { Location_table.view
            ; key_handler
            ; selection = _
            ; set_selection = _
            ; move_selection = _
            }
      =
      Location_table.component
        ~total_allocations
        ~call_sites
        ~rows
        ~presorted
        ~focus
        ~set_focus
        ~on_click_row:set_focus
    in
    let%arr view and key_handler in
    (* Only send keyboard events to this table if it's focused (in the browser sense)
    *)
    let { Keyboard_scope.view; key_help = _ } = Keyboard_scope.wrap ~view ~key_handler in
    view, ()
  ;;
end

module Tab = struct
  module Input = struct
    type t =
      { total_allocations : Byte_units.t Bonsai.Value.t
      ; call_sites : Data.Call_sites.t Bonsai.Value.t
      ; focus : Data.Fragment.t Bonsai.Value.t
      ; set_focus : (Data.Fragment.t -> unit Ui_effect.t) Bonsai.Value.t
      ; hot_locations : Data.Fragment.t list Bonsai.Value.t
      ; hot_paths : Data.Fragment.t list Bonsai.Value.t
      ; toplevel_fragment : Data.Fragment.t Bonsai.Value.t
      ; allocator_fragment : Data.Fragment.t Bonsai.Value.t
      }
  end

  module Output = Unit

  type t =
    | Locations
    | Hot_paths
  [@@deriving sexp, compare, enumerate, equal]

  let name = function
    | Locations -> "Functions"
    | Hot_paths -> "Hot Paths"
  ;;

  let initial = Locations
  let enabled ~input:_ = Bonsai.Value.return (fun (_ : t) -> true)

  let component t ~input ~select_tab:_ =
    let { Input.total_allocations
        ; call_sites
        ; focus
        ; set_focus
        ; hot_locations
        ; hot_paths
        ; toplevel_fragment
        ; allocator_fragment
        }
      =
      input
    in
    match t with
    | Locations ->
      let which_table = Table_tab.Which_table.Locations { hot_locations } in
      Table_tab.component
        ~which_table
        ~total_allocations
        ~call_sites
        ~toplevel_fragment
        ~allocator_fragment
        ~focus
        ~set_focus
    | Hot_paths ->
      let which_table = Table_tab.Which_table.Hot_paths { hot_paths } in
      Table_tab.component
        ~which_table
        ~total_allocations
        ~call_sites
        ~toplevel_fragment
        ~allocator_fragment
        ~focus
        ~set_focus
  ;;
end

type t = { view : Vdom.Node.t }

let component ~trie ~call_sites ~hot_paths ~hot_locations ~focus ~set_focus =
  let open Bonsai.Let_syntax in
  let total_allocations = Data.Fragment_trie.total_allocations <$> trie in
  let toplevel_fragment = Data.Fragment_trie.toplevel_fragment <$> trie in
  let allocator_fragment = Data.Fragment_trie.allocator_fragment <$> trie in
  let input =
    { Tab.Input.total_allocations
    ; call_sites
    ; hot_paths
    ; hot_locations
    ; focus
    ; set_focus
    ; toplevel_fragment
    ; allocator_fragment
    }
  in
  let%sub { Tab_panel.view = tab_panel; _ } = Tab_panel.component ~input (module Tab) in
  let%sub view =
    Panel.panel
      tab_panel
      ~id:"poi-panel"
      ~title:(Value.return "Points of Interest")
      ~collapsible:No
  in
  let%arr view in
  { view }
;;
