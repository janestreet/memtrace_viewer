open! Core_kernel
open Bonsai_web

module type Button = sig
  type t [@@deriving sexp, equal, enumerate]

  val label : t -> string
  val title : t -> string option
end

let component (type a) (module Button : Button with type t = a) ~name ~value ~set_value =
  let open Bonsai.Let_syntax in
  return
    (let%mapn name = name
     and value = value
     and set_value = set_value in
     let open Vdom in
     let render_item button =
       let selected = Option.equal Button.equal (Some button) value in
       let on_click _ = set_value button in
       let label_attrs =
         match Button.title button with
         | Some title -> [ Attr.title title ]
         | None -> []
       in
       Node.li
         []
         [ Node.label
             label_attrs
             [ Node.input
                 (List.concat
                    [ [ Attr.type_ "radio" ]
                    ; [ Attr.name name ]
                    ; [ Attr.on_click on_click ]
                    ; (if selected then [ Attr.checked ] else [])
                    ])
                 []
             ; Node.text (Button.label button)
             ]
         ]
     in
     Node.ul [ Attr.class_ "radio-list" ] (List.map ~f:render_item Button.all))
;;
