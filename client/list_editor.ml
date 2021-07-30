open! Core
open Bonsai_web

module Index_set = struct
  (* Use a map for easy use with [Bonsai.assoc] *)
  type t = unit Int.Map.t [@@deriving equal, sexp, bin_io]
end

let component ?(extra_attrs = []) ?(add_item_text = "Add") here item =
  let open Bonsai.Let_syntax in
  let open Vdom in
  let%sub index_set_state =
    Bonsai.state here (module Index_set) ~default_model:Int.Map.empty
  in
  let%sub next_index_state = Bonsai.state here (module Int) ~default_model:0 in
  let%sub rows =
    let index_set =
      let%map index_set, _ = index_set_state in
      index_set
    in
    Bonsai.assoc
      (module Int)
      index_set
      ~f:(fun index _ ->
        let%sub item = item in
        return
          (let%map item = item
           and index_set, set_index_set = index_set_state
           and index = index in
           let remove = set_index_set (Int.Map.remove index_set index) in
           item
           |> And_view.map_view ~f:(fun view ->
             Node.li
               ~key:(index |> Int.to_string)
               [ Node.button
                   ~attr:
                     (Attr.many_without_merge
                        [ (* Important to set type="button" here: the default is type="submit",
                             which makes Enter delete the first item! *)
                          Attr.type_ "button"
                        ; Attr.on_click (fun _ -> remove)
                        ])
                   [ Node.text "-" ]
               ; Node.text " "
               ; view
               ])))
  in
  return
    (let%map rows = rows
     and index_set, set_index_set = index_set_state
     and next_index, set_next_index = next_index_state in
     let add_item () =
       Vdom.Effect.Many
         [ set_index_set (Int.Map.add_exn index_set ~key:next_index ~data:())
         ; set_next_index (next_index + 1)
         ]
     in
     let add_item_row =
       Vdom.Node.li
         ~key:"add"
         [ Node.button
             ~attr:
               (Attr.many_without_merge
                  [ (* Important so that Enter doesn't click this button *)
                    Attr.type_ "button"
                  ; Attr.on_click (fun _ -> add_item ())
                  ])
             [ Node.text add_item_text ]
         ]
     in
     let open Vdom in
     let values_and_views = Int.Map.data rows in
     let row_views = List.map ~f:And_view.view values_and_views in
     let view =
       Node.ul ~attr:(Attr.many_without_merge extra_attrs) (row_views @ [ add_item_row ])
     in
     let value = List.map ~f:And_view.value values_and_views in
     { And_view.value; view })
;;
