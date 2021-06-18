open! Core
open Bonsai_web

val component
  :  ?extra_attrs:Vdom.Attr.t list
  -> ?add_item_text:string
  -> Lexing.position
  -> 'a And_view.t Bonsai.Computation.t
  -> 'a list And_view.t Bonsai.Computation.t
