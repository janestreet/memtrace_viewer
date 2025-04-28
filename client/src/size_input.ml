open! Core
open Bonsai_web_proc

type t = Byte_units.t option And_view.t

module Unit = struct
  type t =
    | Bytes
    | Kilobytes
    | Megabytes
    | Gigabytes
    | Terabytes
  [@@deriving equal, enumerate, sexp]

  let to_string = function
    | Bytes -> "B"
    | Kilobytes -> "K"
    | Megabytes -> "M"
    | Gigabytes -> "G"
    | Terabytes -> "T"
  ;;

  let float_to_byte_units t f =
    match t with
    | Bytes -> Byte_units.of_bytes_float_exn f
    | Kilobytes -> Byte_units.of_kilobytes f
    | Megabytes -> Byte_units.of_megabytes f
    | Gigabytes -> Byte_units.of_gigabytes f
    | Terabytes -> Byte_units.of_terabytes f
  ;;
end

module Float_option = struct
  type t = float option [@@deriving sexp, equal]
end

let component =
  let open Bonsai.Let_syntax in
  let%sub state = Bonsai.state None ~equal:[%equal: Float_option.t] in
  let%sub unit_state = Bonsai.state Kilobytes ~equal:[%equal: Unit.t] in
  return
    (let%map value, set_value = state
     and unit, set_unit = unit_state in
     let open Vdom in
     let view =
       Node.span
         [ Vdom_input_widgets.Entry.number
             ~allow_updates_when_focused:`Never
             ~merge_behavior:Legacy_dont_merge
             (module Util.Float_html_syntax)
             ~value
             ~on_input:set_value
             ~step:0.1
             ~call_on_input_when:Text_changed
             ~extra_attrs:[ Attr.min 0.0 ]
         ; Node.text " "
         ; Vdom_input_widgets.Dropdown.of_enum
             ~merge_behavior:Legacy_dont_merge
             (module Unit)
             ~selected:unit
             ~on_change:set_unit
         ]
     in
     let value = value |> Option.map ~f:(Unit.float_to_byte_units unit) in
     { And_view.value; view })
;;
