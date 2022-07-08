open! Core
open Bonsai_web

module String_option = struct
  type t = string option [@@deriving sexp, equal]
end

let component =
  let open Bonsai.Let_syntax in
  let%sub state = Bonsai.state (module String_option) ~default_model:None in
  return
    (let%map value, set_value = state in
     let view = Vdom_input_widgets.Entry.text ~value ~on_input:set_value () in
     { And_view.value; view })
;;
