open! Core
open Bonsai_web_proc

module Index_set = struct
  (* Use a map for easy use with [Bonsai.assoc] *)
  type t = unit Int.Map.t [@@deriving equal, sexp, bin_io]
end

let component
  ?(attr = Vdom.Attr.empty)
  ?(add_button_attr = Vdom.Attr.empty)
  ?(remove_button_attr = Vdom.Attr.empty)
  ?(add_item_text = "Add")
  item
  =
  let open Bonsai.Let_syntax in
  let open Vdom in
  let%sub index_set_state = Bonsai.state Int.Map.empty ~equal:[%equal: Index_set.t] in
  let%sub next_index_state = Bonsai.state 0 ~equal:[%equal: Int.t] in
  let%sub rows =
    let index_set =
      let%map index_set, _ = index_set_state in
      index_set
    in
    Bonsai.assoc (module Int) index_set ~f:(fun index _ ->
      let%sub item in
      return
        (let%map item
         and index_set, set_index_set = index_set_state
         and index in
         let remove = set_index_set (Map.remove (index_set : _ Int.Map.t) index) in
         let glyph = Node.text "−" (* U+2212 MINUS SIGN (bigger than hyphen) *) in
         item
         |> And_view.map_view ~f:(fun view ->
           Node.li
             ~key:(index |> Int.to_string)
             [ Node.button
                 ~attrs:
                   [ (* Important to set type="button" here: the default is type="submit",
                        which makes Enter delete the first item! *)
                     Attr.type_ "button"
                   ; Attr.class_ "list-editor-remove-button"
                   ; Attr.on_click (fun _ -> remove)
                   ; remove_button_attr
                   ]
                 [ glyph ]
             ; Node.text " "
             ; view
             ])))
  in
  return
    (let%map rows
     and index_set, set_index_set = index_set_state
     and next_index, set_next_index = next_index_state in
     let add_item () =
       Vdom.Effect.Many
         [ set_index_set (Map.add_exn (index_set : _ Int.Map.t) ~key:next_index ~data:())
         ; set_next_index (next_index + 1)
         ]
     in
     let add_item_row =
       Vdom.Node.li
         ~key:"add"
         [ Node.button
             ~attrs:
               [ (* Important so that Enter doesn't click this button *)
                 Attr.type_ "button"
               ; Attr.class_ "list-editor-add-item-button"
               ; Attr.on_click (fun _ -> add_item ())
               ; add_button_attr
               ]
             [ Node.text add_item_text ]
         ]
     in
     let open Vdom in
     let values_and_views = Map.data (rows : _ Int.Map.t) in
     let row_views = List.map ~f:And_view.view values_and_views in
     let view = Node.ul ~attrs:[ attr ] (row_views @ [ add_item_row ]) in
     let value = List.map ~f:And_view.value values_and_views in
     { And_view.value; view })
;;
