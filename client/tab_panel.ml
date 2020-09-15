open! Core_kernel
open Bonsai_web
include Tab_panel_intf

module Component (Tab : Tab) = struct
  module Tab_bar = struct
    type t =
      { selected_tab : Tab.t
      ; select_tab : Tab.t -> Vdom.Event.t
      ; view : Vdom.Node.t
      }

    let name = "Tab bar"

    module Input = Unit

    module Result = struct
      type nonrec t = t
    end

    module Model = struct
      type t = { selected_tab : Tab.t } [@@deriving sexp]

      let equal t1 t2 = Tab.compare t1.selected_tab t2.selected_tab = 0
    end

    module Action = struct
      type t = Select_tab of Tab.t [@@deriving sexp_of]
    end

    let apply_action ~inject:_ ~schedule_event:_ _input _model action : Model.t =
      match action with
      | Action.Select_tab tab -> { selected_tab = tab }
    ;;

    let compute ~inject _input (model : Model.t) : Result.t =
      let selected_tab = model.selected_tab in
      let select_tab tab = inject (Action.Select_tab tab) in
      let view =
        let open Vdom in
        let tab_view tab =
          let classes = if Tab.compare tab selected_tab = 0 then [ "selected" ] else [] in
          Node.li
            [ Attr.classes classes ]
            [ Node.button
                [ Attr.on_click (fun _ -> select_tab tab) ]
                [ Node.text (Tab.name tab) ]
            ]
        in
        Node.ul [ Attr.class_ "tab-bar" ] (List.map ~f:tab_view Tab.all)
      in
      { selected_tab; select_tab; view }
    ;;
  end

  let tab_bar : Tab_bar.Result.t Bonsai.Computation.t =
    Bonsai.of_module0 (module Tab_bar) ~default_model:{ selected_tab = Tab.initial }
  ;;

  let current_tab
        ~(input : Tab.Input.t Bonsai.Value.t)
        ~(tab_bar : Tab_bar.t Bonsai.Value.t)
    : (Vdom.Node.t * Tab.Result.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let selected_tab =
      let%map tab_bar = tab_bar in
      tab_bar.selected_tab
    in
    let select_tab =
      let%map tab_bar = tab_bar in
      tab_bar.select_tab
    in
    let with_ tab = Tab.component tab ~input ~select_tab in
    Bonsai.enum (module Tab) ~match_:selected_tab ~with_
  ;;

  let view tab_bar_view current_tab_view =
    let open Vdom in
    Node.div [ Attr.class_ "tab-panel" ] [ tab_bar_view; current_tab_view ]
  ;;

  let component (input : Tab.Input.t Bonsai.Value.t)
    : (Vdom.Node.t * Tab.Result.t) Bonsai.Computation.t
    =
    let open Bonsai.Let_syntax in
    let%sub tab_bar = tab_bar in
    let%sub current_tab = current_tab ~input ~tab_bar in
    return
      (let%map { view = tab_bar_view; _ } = tab_bar
       and current_tab_view, result = current_tab in
       view tab_bar_view current_tab_view, result)
  ;;
end

let component
      (type input result)
      (module Tab : Tab with type Input.t = input and type Result.t = result)
  =
  let module Tab_panel = Component (Tab) in
  Tab_panel.component
;;
