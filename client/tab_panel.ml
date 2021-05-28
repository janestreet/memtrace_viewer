open! Core_kernel
open Bonsai_web
include Tab_panel_intf

type ('tab, 'extra) t =
  { view : Vdom.Node.t
  ; selected_tab : 'tab
  ; select_tab : 'tab -> Vdom.Event.t
  ; extra : 'extra
  }

module Component (Tab : Tab) = struct
  module Tab_bar = struct
    type t =
      { selected_tab : Tab.t
      ; select_tab : Tab.t -> Vdom.Event.t
      ; view : Vdom.Node.t
      }

    module State = struct
      type t = { selected_tab : Tab.t } [@@deriving sexp]

      let equal t1 t2 = Tab.compare t1.selected_tab t2.selected_tab = 0
    end

    let component ~(tab_is_enabled : (Tab.t -> bool) Bonsai.Value.t)
      : t Bonsai.Computation.t
      =
      let open Bonsai.Let_syntax in
      let%sub state =
        Bonsai.state [%here] (module State) ~default_model:{ selected_tab = Tab.initial }
      in
      return
        (let%map state, set_state = state
         and tab_is_enabled = tab_is_enabled in
         let selected_tab = state.selected_tab in
         let select_tab tab = set_state { selected_tab = tab } in
         let view =
           let open Vdom in
           let tab_view tab =
             let classes =
               if Tab.compare tab selected_tab = 0 then [ "selected" ] else []
             in
             let on_click_or_disabled =
               if tab_is_enabled tab
               then Attr.on_click (fun _ -> select_tab tab)
               else Attr.disabled
             in
             Node.li
               [ Attr.classes classes ]
               [ Node.button [ on_click_or_disabled ] [ Node.text (Tab.name tab) ] ]
           in
           Node.ul [ Attr.class_ "tab-bar" ] (List.map ~f:tab_view Tab.all)
         in
         { selected_tab; select_tab; view })
    ;;
  end

  let current_tab ~(tab_bar : Tab_bar.t Bonsai.Value.t)
    : (Vdom.Node.t * Tab.Extra.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub { Tab_bar.select_tab; selected_tab; _ } = Bonsai.read tab_bar in
    Bonsai.enum
      (module Tab)
      ~match_:selected_tab
      ~with_:(fun tab -> Tab.component tab ~select_tab)
  ;;

  let view tab_bar_view current_tab_view =
    let open Vdom in
    Node.div [ Attr.class_ "tab-panel" ] [ tab_bar_view; current_tab_view ]
  ;;

  let component : (Tab.t, Tab.Extra.t) t Bonsai.Computation.t =
    let open Bonsai.Let_syntax in
    let%sub tab_is_enabled = Tab.enabled in
    let%sub tab_bar = Tab_bar.component ~tab_is_enabled in
    let%sub current_tab = current_tab ~tab_bar in
    return
      (let%map { view = tab_bar_view; selected_tab; select_tab } = tab_bar
       and current_tab_view, extra = current_tab in
       let view = view tab_bar_view current_tab_view in
       { view; select_tab; selected_tab; extra })
  ;;
end

let component
      (type tab extra)
      (module Tab : Tab with type t = tab and type Extra.t = extra)
  =
  let module Tab_panel = Component (Tab) in
  Tab_panel.component
;;
