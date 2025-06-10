open! Core
open Bonsai_web_proc
include Tab_panel_intf

type ('tab, 'output) t =
  { view : Vdom.Node.t
  ; selected_tab : 'tab
  ; select_tab : 'tab -> unit Vdom.Effect.t
  ; output : 'output
  }

module Component (Tab : Tab) = struct
  module Tab_bar = struct
    type t =
      { selected_tab : Tab.t
      ; select_tab : Tab.t -> unit Vdom.Effect.t
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
        Bonsai.state { selected_tab = Tab.initial } ~equal:[%equal: State.t]
      in
      return
        (let%map state, set_state = state
         and tab_is_enabled in
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
               ~attrs:[ Attr.classes classes ]
               [ Node.button
                   ~attrs:[ Attr.class_ "flat-button"; on_click_or_disabled ]
                   [ Node.text (Tab.name tab) ]
               ]
           in
           Node.ul ~attrs:[ Attr.class_ "tab-bar" ] (List.map ~f:tab_view Tab.all)
         in
         { selected_tab; select_tab; view })
    ;;
  end

  let current_tab ~input ~(tab_bar : Tab_bar.t Bonsai.Value.t)
    : (Vdom.Node.t * Tab.Output.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub { Tab_bar.select_tab; selected_tab; _ } = Bonsai.read tab_bar in
    Bonsai.enum (module Tab) ~match_:selected_tab ~with_:(fun tab ->
      Tab.component tab ~input ~select_tab)
  ;;

  let view tab_bar_view current_tab_view =
    let open Vdom in
    Node.div
      ~attrs:[ Attr.class_ "tab-panel" ]
      [ tab_bar_view
      ; Node.div ~attrs:[ Attr.class_ "tab-panel-content" ] [ current_tab_view ]
      ]
  ;;

  let component : input:Tab.Input.t -> (Tab.t, Tab.Output.t) t Bonsai.Computation.t =
    fun ~input ->
    let open Bonsai.Let_syntax in
    let%sub tab_bar = Tab_bar.component ~tab_is_enabled:(Tab.enabled ~input) in
    let%sub current_tab = current_tab ~input ~tab_bar in
    return
      (let%map { view = tab_bar_view; selected_tab; select_tab } = tab_bar
       and current_tab_view, output = current_tab in
       let view = view tab_bar_view current_tab_view in
       { view; select_tab; selected_tab; output })
  ;;
end

let component
  (type tab input output)
  (module Tab : Tab with type t = tab and type Input.t = input and type Output.t = output)
  =
  let module Tab_panel = Component (Tab) in
  Tab_panel.component
;;
