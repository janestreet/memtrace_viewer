open! Core
open Bonsai_simple_table_intf
open Bonsai_web_proc
open Incr.Let_syntax
module Col_group = Col_group

module type Id = Id
module type Row = Row
module type S = S

module Make (Row : Row) (Col_id : Id) = struct
  type cell =
    { node : Vdom.Node.t
    ; attrs : Vdom.Attr.t list
    }

  module T = struct
    module Column = struct
      module Renderer = struct
        type t = Row.Id.t -> Row.t -> cell

        let equal = phys_equal
      end

      type t =
        { header : cell
        ; header_for_testing : string option
        ; render : Renderer.t
        ; group : Col_group.t option
        ; classes : string list
        }

      let create ?header_for_testing ?(classes = []) ~header ~render ~group () =
        { header_for_testing; header; render; group; classes }
      ;;
    end

    module Input = struct
      type t =
        { rows : Row.t Row.Id.Map.t
        ; cols : Column.t Col_id.Map.t
        ; row_ids_in_order : [ `All_in_default_order | `These of Row.Id.t list ]
        ; col_ids_in_order : Col_id.t list
        ; table_attrs : Vdom.Attr.t list
        ; percentage_rendered : Percent.t
        }
      [@@deriving fields ~getters ~iterators:create]

      let create
        ?(percentage_rendered = Percent.one_hundred_percent)
        ~rows
        ~cols
        ~row_ids_in_order
        ~col_ids_in_order
        ~table_attrs
        ()
        =
        Fields.create
          ~percentage_rendered
          ~rows
          ~cols
          ~row_ids_in_order
          ~col_ids_in_order
          ~table_attrs
      ;;
    end

    module Model = struct
      type t = { focus_row : Row.Id.t option }
      [@@deriving fields ~getters, equal, sexp_of]

      let create () = { focus_row = None }
    end

    module Action = struct
      type t =
        | Set_focus_row of Row.Id.t option
        | Move_focus of [ `Prev | `Next ]
      [@@deriving sexp_of]
    end

    module Focus = struct
      type elt =
        { after : Row.Id.t option
        ; before : Row.Id.t option
        }
      [@@deriving compare]

      type t =
        { moves : elt Row.Id.Map.t
        ; first : Row.Id.t option
        ; last : Row.Id.t option
        }
      [@@deriving compare]

      let create_all_rows rows =
        let key_set = Incr.Map.keys rows in
        let%map moves =
          Incr.Map.unordered_fold
            rows
            ~data_equal:(fun _ _ -> true)
            ~init:Row.Id.Map.empty
            ~add:(fun ~key ~data:_ t ->
              let before =
                Option.map (Map.closest_key t `Less_than key) ~f:(fun (key', elt) ->
                  key', { elt with after = Some key })
              in
              let after =
                Option.map (Map.closest_key t `Greater_than key) ~f:(fun (key', elt) ->
                  key', { elt with before = Some key })
              in
              let set_opt thing map =
                match thing with
                | None -> map
                | Some (key, new_elt) -> Map.set map ~key ~data:new_elt
              in
              t
              |> set_opt before
              |> set_opt after
              |> Map.set
                   ~key
                   ~data:
                     { after = Option.map after ~f:fst
                     ; before = Option.map before ~f:fst
                     })
            ~remove:(fun ~key ~data:_ t ->
              let { before; after } = Map.find_exn t key in
              let t =
                match before with
                | None -> t
                | Some before ->
                  Map.update t before ~f:(fun elt ->
                    let elt = Option.value_exn elt in
                    { elt with after })
              in
              match after with
              | None -> t
              | Some after ->
                Map.update t after ~f:(fun elt ->
                  let elt = Option.value_exn elt in
                  { elt with before }))
        and first = key_set >>| Set.min_elt
        and last = key_set >>| Set.max_elt in
        { moves; first; last }
      ;;

      let create_these_rows ids =
        let%map ids in
        let moves =
          let rec loop (t, before) = function
            | [] -> t
            | key :: rest ->
              loop (Map.set t ~key ~data:{ before; after = List.hd rest }, Some key) rest
          in
          loop (Row.Id.Map.empty, None) ids
        in
        { moves; first = List.hd ids; last = List.last ids }
      ;;

      let create (input : Input.t Incr.t) =
        let t =
          match%pattern_bind input >>| Input.row_ids_in_order with
          | `All_in_default_order -> create_all_rows (input >>| Input.rows)
          | `These ids -> create_these_rows ids
        in
        Incr.set_cutoff t (Incr.Cutoff.of_compare [%compare: t]);
        t
      ;;

      let move t focus ~dir =
        let moves =
          let%bind.Option key = focus in
          Map.find t.moves key
        in
        match moves with
        | None ->
          (* If there's no focus or the focus is invalid, we grab the extreme element,
             depending on the direction of movement. *)
          (match dir with
           | `Prev -> t.last
           | `Next -> t.first)
        | Some { before; after } ->
          (* If we are focused, then just move to the next key in the map. *)
          (match dir with
           | `Prev -> before
           | `Next -> after)
      ;;
    end

    let serialize_row_id row_id =
      Sexp.to_string_mach (Row.Id.sexp_of_t row_id)
      |> String.map ~f:(function
        (* This mapping probably maps two inputs to the same output, but this is unlikely
           enough that it should be fine. *)
        | '\"' -> '_'
        | c -> c)
    ;;

    let scroll_to_row_effect =
      Effect.of_sync_fun (fun row_id ->
        if not am_running_test
        then (
          let row_id = serialize_row_id row_id in
          let open Js_of_ocaml in
          match
            Dom_html.document##querySelector
              (Js.string [%string "[data-row-id=\"%{row_id}\"]"])
            |> Js.Opt.to_option
          with
          | Some element ->
            let scrollable =
              (Js.Unsafe.coerce element
               : < scrollIntoViewIfNeeded : bool Js.t -> unit Js.meth > Js.t)
            in
            scrollable##scrollIntoViewIfNeeded (Js.bool false)
          | None -> ()))
    ;;

    let apply_action context focus { Model.focus_row } action =
      match focus with
      | Bonsai.Computation_status.Active focus ->
        let focus_row =
          match (action : Action.t) with
          | Set_focus_row focus_row -> focus_row
          | Move_focus dir -> Focus.move focus focus_row ~dir
        in
        Option.iter focus_row ~f:(fun row_id ->
          Bonsai.Apply_action_context.schedule_event context (scroll_to_row_effect row_id));
        { Model.focus_row }
      | Inactive ->
        eprint_s
          [%message
            [%here]
              "An action sent to a [state_machine1] has been dropped because its input \
               was not present. This happens when the [state_machine1] is inactive when \
               it receives a message."
              (action : Action.t)];
        { Model.focus_row }
    ;;

    module Rendered = struct
      type col =
        { header : cell
        ; header_for_testing : string option
        ; id : Col_id.t
        ; classes : string list
        }

      type col_group =
        { group : Col_group.t option
        ; cols_in_group : col list
        }

      type row =
        { focused : bool
        ; cells : cell Col_id.Map.t Lazy.t
        }

      type t =
        { cols : [ `With_groups of col_group list | `Without_groups of col list ] Incr.t
        ; rendered_rows : row Row.Id.Map.t Incr.t
        ; unrendered_rows_length : int Incr.t
        ; row_ids_in_order : Row.Id.t list Incr.t
        ; table_attrs : Vdom.Attr.t list Incr.t
        }

      let create input model =
        let cols = input >>| Input.cols in
        let cols_in_order =
          let%map cols
          and col_ids_in_order = input >>| Input.col_ids_in_order in
          List.filter_map col_ids_in_order ~f:(fun id ->
            let%map.Option c = Map.find cols id in
            id, c)
        in
        let focused_row = model >>| Model.focus_row in
        let row_ids_in_order =
          match%pattern_bind input >>| Input.row_ids_in_order with
          | `These rows -> rows
          | `All_in_default_order -> input >>| Input.rows >>| Map.keys
        in
        let rows =
          let%bind column_renderers =
            Incr_map.map
              cols
              (* This funky looking data_equal function is used to basically provide a
                 cutoff for the data that we're looking to extract. In this case, the
                 renderer function. *)
              ~data_equal:(fun a b -> Column.Renderer.equal a.Column.render b.render)
              ~f:(fun a -> a.render)
          in
          Incr.Map.mapi (input >>| Input.rows) ~f:(fun ~key:row_id ~data:row ->
            let cells =
              lazy (Map.map column_renderers ~f:(fun render -> render row_id row))
            in
            { cells; focused = false })
        in
        let%pattern_bind row_ids_in_order, unrendered_rows_length =
          let%map row_ids_in_order
          and percentage_rendered = input >>| Input.percentage_rendered in
          if Percent.(percentage_rendered = one_hundred_percent)
          then row_ids_in_order, 0
          else (
            let take =
              Percent.to_mult percentage_rendered
              *. Int.to_float (List.length row_ids_in_order)
              |> Float.round_down
              |> Float.to_int
            in
            List.take row_ids_in_order take, List.length row_ids_in_order - take)
        in
        let rendered_rows =
          (* after everything else, just come in and tweak the one element that has focus. *)
          let%map rows and focused_row in
          match focused_row with
          | None -> rows
          | Some focus ->
            Map.change
              rows
              focus
              ~f:(Option.map ~f:(fun { cells; _ } -> { cells; focused = true }))
        in
        let cols =
          let%map cols_in_order in
          let should_include_groups =
            List.exists cols_in_order ~f:(fun (_col_id, c) ->
              Option.is_some c.Column.group)
          in
          let mk_col (id, c) =
            { header = c.Column.header
            ; classes = c.classes
            ; header_for_testing = c.header_for_testing
            ; id
            }
          in
          if should_include_groups
          then (
            let grouped_cols =
              List.group cols_in_order ~break:(fun (_cid1, c1) (_cid2, c2) ->
                not ([%compare.equal: Col_group.t option] c1.group c2.group))
            in
            `With_groups
              (List.map grouped_cols ~f:(fun cols ->
                 let group =
                   let _id, first_col = List.hd_exn cols in
                   first_col.group
                 in
                 { group; cols_in_group = List.map cols ~f:mk_col })))
          else `Without_groups (List.map cols_in_order ~f:mk_col)
        in
        { table_attrs = input >>| Input.table_attrs
        ; rendered_rows
        ; unrendered_rows_length
        ; row_ids_in_order
        ; cols
        }
      ;;

      let view_for_testing t =
        let%map cols = t.cols
        and rendered_rows = t.rendered_rows
        and row_ids_in_order = t.row_ids_in_order in
        lazy
          (let node_to_string node =
             Virtual_dom_test_helpers.Node_helpers.(
               unsafe_convert_exn node |> to_string_html)
           in
           let columns =
             let focus_col =
               Ascii_table_kernel.Column.create "\nfocus" (fun row ->
                 if row.focused then "*" else " ")
             in
             let ascii_table_col { id; header; header_for_testing; classes = _ } ~group =
               let header =
                 (match group with
                  | None -> ""
                  | Some g -> Col_group.to_string g)
                 ^ "\n"
                 ^ Option.value header_for_testing ~default:(node_to_string header.node)
               in
               Ascii_table_kernel.Column.create header (fun (row : row) ->
                 node_to_string (Map.find_exn (Lazy.force row.cells) id).node)
             in
             let data_cols =
               match cols with
               | `Without_groups cols -> List.map cols ~f:(ascii_table_col ~group:None)
               | `With_groups groups ->
                 List.concat_map groups ~f:(fun { group; cols_in_group } ->
                   List.map cols_in_group ~f:(ascii_table_col ~group))
             in
             focus_col :: data_cols
           in
           let rows = List.filter_map row_ids_in_order ~f:(Map.find rendered_rows) in
           Ascii_table_kernel.draw
             columns
             rows
             ~limit_width_to:3000
             ~prefer_split_on_spaces:false
           |> Option.value_exn
           |> Ascii_table_kernel.Screen.to_string
                ~bars:`Unicode
                ~string_with_attr:(fun _attr str -> str))
      ;;

      let col_ids_in_order t =
        match%pattern_bind t.cols with
        | `Without_groups cols -> cols >>| List.map ~f:(fun { id; _ } -> id)
        | `With_groups groups ->
          groups
          >>| List.concat_map ~f:(fun { group = _; cols_in_group } ->
            List.map cols_in_group ~f:(fun { id; _ } -> id))
      ;;

      (* use "\u
         {0 a0}
         " (aka &nbsp;) to get the rows to render and take up vertical space on the page
         even when empty. *)
      let empty_row = Vdom.Node.tr [ Vdom.Node.td [ Vdom.Node.text "\u{00a0}" ] ]

      let view t ~inject =
        let col_ids_in_order = col_ids_in_order t in
        Incr.set_cutoff col_ids_in_order (Incr.Cutoff.of_equal [%equal: Col_id.t list]);
        let%map rows =
          let%bind col_ids_in_order in
          let%map rendered_rows_by_key =
            Incr.Map.mapi t.rendered_rows ~f:(fun ~key:row_id ~data:row ->
              let { cells; focused } = row in
              Vdom.Node.lazy_
                (Lazy.map cells ~f:(fun cells ->
                   let cells =
                     Map.mapi cells ~f:(fun ~key:_col_id ~data:{ node; attrs } ->
                       Vdom.Node.td ~attrs:[ Vdom.Attr.many_without_merge attrs ] [ node ])
                   in
                   let focus_attr =
                     if focused then Vdom.Attr.class_ "focused" else Vdom.Attr.empty
                   in
                   let on_click_attr =
                     Vdom.Attr.on_click (fun _ev ->
                       inject (Action.Set_focus_row (Some row_id)))
                   in
                   Vdom.Node.tr
                     ~attrs:
                       [ Vdom.Attr.(
                           on_click_attr
                           @ focus_attr
                           (* We use "data-row-id" instead of "id" because it can handle a
                              wider range of strings, which is necessary for handling
                              sexps. In addition, we also want to avoid collisions with
                              existing "id"s *)
                           @ create "data-row-id" (serialize_row_id row_id))
                       ]
                     (List.map col_ids_in_order ~f:(fun col_id ->
                        Map.find_exn cells col_id)))))
          and keys = t.row_ids_in_order
          and unrendered_rows_length = t.unrendered_rows_length in
          let unrendered_rows =
            List.init unrendered_rows_length ~f:(fun _ -> empty_row)
          in
          List.filter_map keys ~f:(Map.find rendered_rows_by_key) @ unrendered_rows
        and col_defn_tags, header_rows =
          match%map t.cols with
          | `Without_groups cols ->
            ( List.map cols ~f:(fun c ->
                Vdom.Node.create "col" ~attrs:[ Vdom.Attr.classes c.classes ] [])
            , [ Vdom.Node.tr
                  (List.map cols ~f:(fun c ->
                     Vdom.Node.th
                       ~attrs:[ Vdom.Attr.many_without_merge c.header.attrs ]
                       [ c.header.node ]))
              ] )
          | `With_groups groups ->
            let col_defn_tags, groups_row, col_headers_row =
              List.fold
                groups
                ~init:([], [], [])
                ~f:
                  (fun
                    (col_defn_tags, groups_row, col_headers_row)
                    { group; cols_in_group }
                  ->
                  let num_cols = List.length cols_in_group in
                  let colgroup_tag =
                    Vdom.Node.create
                      "colgroup"
                      ~attrs:[ Vdom.Attr.create "span" (Int.to_string num_cols) ]
                      (List.map cols_in_group ~f:(fun column ->
                         Vdom.Node.create
                           "col"
                           ~attrs:[ Vdom.Attr.classes column.classes ]
                           []))
                  in
                  let group_th =
                    let group_name =
                      match group with
                      | None -> Vdom.Node.none_deprecated [@alert "-deprecated"]
                      | Some group_name -> Vdom.Node.text (Col_group.to_string group_name)
                    in
                    Vdom.Node.th
                      ~attrs:[ Vdom.Attr.create "colspan" (Int.to_string num_cols) ]
                      [ group_name ]
                  in
                  let col_headers_ths =
                    let last_idx = num_cols - 1 in
                    List.mapi cols_in_group ~f:(fun idx c ->
                      let classes =
                        [ Option.some_if (idx = 0) "simple-table-first-header-in-group"
                        ; Option.some_if
                            (idx = last_idx)
                            "simple-table-last-header-in-group"
                        ]
                        |> List.filter_opt
                        |> Vdom.Attr.classes
                      in
                      let attrs =
                        Vdom.Attrs.merge_classes_and_styles (classes :: c.header.attrs)
                      in
                      Vdom.Node.th
                        ~attrs:[ Vdom.Attr.many_without_merge attrs ]
                        [ c.header.node ])
                  in
                  ( col_defn_tags @ [ colgroup_tag ]
                  , groups_row @ [ group_th ]
                  , col_headers_row @ col_headers_ths ))
            in
            ( col_defn_tags
            , [ Vdom.Node.tr
                  ~attrs:[ Vdom.Attr.class_ "simple-table-header-group-row" ]
                  groups_row
              ; Vdom.Node.tr
                  ~attrs:[ Vdom.Attr.class_ "simple-table-header-main-row" ]
                  col_headers_row
              ] )
        and table_attrs = t.table_attrs in
        Vdom.Node.table
          ~attrs:[ Vdom.Attr.many_without_merge table_attrs ]
          (col_defn_tags @ [ Vdom.Node.thead header_rows; Vdom.Node.tbody rows ])
      ;;
    end

    let key_handler ~(inject : Action.t -> _) =
      let open Vdom_keyboard in
      let command ?cond ?group ~keys ~description f =
        let handler =
          let open Keyboard_event_handler.Handler in
          match cond with
          | None -> with_prevent_default f
          | Some cond -> only_handle_if cond f ~prevent_default:()
        in
        { Keyboard_event_handler.Command.keys; description; group; handler }
      in
      let key = Vdom_keyboard.Keystroke.create' in
      Keyboard_event_handler.of_command_list_exn
        [ command
            ~cond:Keyboard_event_handler.Condition.(not_ has_input_target)
            ~keys:[ key KeyJ; key ArrowDown ]
            ~description:"Move focus down"
            (fun _ev -> inject (Move_focus `Next))
        ; command
            ~cond:Keyboard_event_handler.Condition.(not_ has_input_target)
            ~keys:[ key KeyK; key ArrowUp ]
            ~description:"Move focus up"
            (fun _ev -> inject (Move_focus `Prev))
        ]
    ;;

    let focus_row input model =
      let%map rows = input >>| Input.rows
      and focus_row_id = model >>| Model.focus_row in
      let%bind.Option id = focus_row_id in
      let%map.Option row = Map.find rows id in
      id, row
    ;;

    module Result = struct
      type t =
        { view : Vdom.Node.t
        ; view_for_testing : string Lazy.t
        ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
        ; focus_row : (Row.Id.t * Row.t) option
        ; inject : Action.t -> unit Vdom.Effect.t
        }
    end

    let compute input model ~inject =
      let key_handler = key_handler ~inject in
      let rendered = Rendered.create input model in
      let%map view = Rendered.view rendered ~inject
      and view_for_testing = Rendered.view_for_testing rendered
      and focus_row = focus_row input model in
      { Result.view; view_for_testing; key_handler; focus_row; inject }
    ;;
  end

  include T
  open Bonsai.Let_syntax

  let bonsai input =
    let%sub focus = Bonsai.Incr.compute input ~f:Focus.create in
    let%sub model, inject =
      Bonsai.state_machine_with_input
        ~equal:[%equal: Model.t]
        ~sexp_of_action:[%sexp_of: Action.t]
        ~default_model:(Model.create ())
        ~apply_action
        focus
    in
    Bonsai.Incr.compute
      (Tuple3.create <$> input <*> model <*> inject)
      ~f:(fun params ->
        let%pattern_bind.Incr input, model, inject = params in
        let%bind.Incr inject in
        T.compute input model ~inject)
  ;;
end
