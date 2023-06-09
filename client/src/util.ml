open! Core
open Bonsai_web

module Float_html_syntax : Stringable.S with type t = float = struct
  type t = float

  let to_string x = sprintf "%g" x
  let of_string = Float.of_string
end

let placeholder_span = Vdom.Node.span []
let placeholder_div = Vdom.Node.div []
let placeholder_svg = Virtual_dom_svg.Node.g []

module Option_model (T : Bonsai.Model) : Bonsai.Model with type t = T.t option = struct
  type t = T.t option [@@deriving sexp_of]
end
