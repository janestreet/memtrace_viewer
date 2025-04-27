open! Core
open Bonsai_web_proc

module type Tab = sig
  type t [@@deriving enumerate, sexp, compare, equal]

  module Input : T
  module Output : T

  val name : t -> string
  val initial : t

  val component
    :  t
    -> input:Input.t
    -> select_tab:(t -> unit Vdom.Effect.t) Bonsai.Value.t
    -> (Vdom.Node.t * Output.t) Bonsai.Computation.t

  val enabled : input:Input.t -> (t -> bool) Bonsai.Value.t
end

module type Tab_panel = sig
  module type Tab = Tab

  type ('tab, 'output) t =
    { view : Vdom.Node.t
    ; selected_tab : 'tab
    ; select_tab : 'tab -> unit Vdom.Effect.t
    ; output : 'output
    }

  val component
    :  (module Tab with type t = 'tab and type Input.t = 'input and type Output.t = 'output)
    -> input:'input
    -> ('tab, 'output) t Bonsai.Computation.t
end
