open! Core
open Bonsai_web_proc

val component
  :  ?attr:Vdom.Attr.t
  -> ?add_button_attr:Vdom.Attr.t
  -> ?remove_button_attr:Vdom.Attr.t
  -> ?add_item_text:string
  -> 'a And_view.t Bonsai.Computation.t
  -> 'a list And_view.t Bonsai.Computation.t
