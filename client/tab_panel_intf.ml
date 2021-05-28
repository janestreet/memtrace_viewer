open! Core_kernel
open Bonsai_web


module type Tab = sig
  type t [@@deriving enumerate, sexp, compare, equal]

  module Extra : T

  val name : t -> string
  val initial : t

  val component
    :  t
    -> select_tab:(t -> Vdom.Event.t) Bonsai.Value.t
    -> (Vdom.Node.t * Extra.t) Bonsai.Computation.t

  val enabled : (t -> bool) Bonsai.Computation.t
end

module type Tab_panel = sig
  module type Tab = Tab

  type ('tab, 'extra) t =
    { view : Vdom.Node.t
    ; selected_tab : 'tab
    ; select_tab : 'tab -> Vdom.Event.t
    ; extra : 'extra
    }

  val component
    :  (module Tab with type t = 'tab and type Extra.t = 'extra)
    -> ('tab, 'extra) t Bonsai.Computation.t
end
