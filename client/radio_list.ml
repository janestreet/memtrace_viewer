open! Core_kernel
open Bonsai_web

module type Button = sig
  type t [@@deriving sexp, equal, enumerate]

  val label : t -> string
  val title : t -> string option
end

type 'a t =
  { view : Vdom.Node.t
  ; value : 'a
  ; changing : bool
  ; reset_changing : Vdom.Event.t
  }
[@@deriving fields]

let component (type a) (module Button : Button with type t = a) ~name ~initial_value =
  let open Bonsai.Let_syntax in
  let%sub changing = Changing.component (module Button) ~initial_value in
  return
    (let%map changing = changing in
     let value = Changing.value changing in
     let set_value = Changing.set_value changing in
     let reset_changing = Changing.set_changing changing false in
     let changing = Changing.changing changing in
     let open Vdom in
     let render_item button =
       let selected = Button.equal button value in
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
     let view =
       Node.ul [ Attr.class_ "radio-list" ] (List.map ~f:render_item Button.all)
     in
     { view; value; changing; reset_changing })
;;
