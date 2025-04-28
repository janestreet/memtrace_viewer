open! Core
open Bonsai_web_proc

module String_option = struct
  type t = string option [@@deriving sexp, equal]
end

let component =
  let open Bonsai.Let_syntax in
  let%sub state = Bonsai.state None ~equal:[%equal: String_option.t] in
  return
    (let%map value, set_value = state in
     let view =
       Vdom_input_widgets.Entry.text
         ~allow_updates_when_focused:`Never
         ~merge_behavior:Legacy_dont_merge
         ~value
         ~on_input:set_value
         ()
     in
     { And_view.value; view })
;;
