open! Core
open Bonsai_web_proc

module Node_type = struct
  type t =
    | Function
    | Allocation_site
end

module type Graph = sig
  type t
  type graph := t

  module Node : sig
    type t

    val type_ : graph:graph -> t -> Node_type.t
    val label : graph:graph -> t -> string
    val details : graph:graph -> t -> string
    val size : graph:graph -> t -> float
  end

  module Tree : sig
    type t

    val node : t -> Node.t
    val children : graph:graph -> t -> t list
    val parent : graph:graph -> t -> t option
    val same : t -> t -> bool
  end

  module Sequence : sig
    type t

    val node : t -> Node.t
    val label : graph:graph -> t -> string list
    val next : graph:graph -> t -> t option
    val prev : graph:graph -> t -> t option
    val same : t -> t -> bool
  end

  val flame_tree : t -> Tree.t list
  val icicle_tree : t -> Tree.t list
  val focus : t -> Sequence.t option
  val size : t -> float
end

module Direction = struct
  type t =
    | Left
    | Right
    | Up
    | Down
end

(* An action that can be performed on the selection. May be disabled, depending on the
   selection. In most cases, this corresponds to a button that can be clicked. *)
module Command = struct
  type t =
    | Enabled of unit Effect.t
    | Disabled
end

module Commands = struct
  type t =
    { extend_focus_to : Command.t
    ; retract_callers_from_focus : Command.t
    ; retract_callees_from_focus : Command.t
    }

  let all_disabled =
    { extend_focus_to = Disabled
    ; retract_callers_from_focus = Disabled
    ; retract_callees_from_focus = Disabled
    }
  ;;
end

module type S = sig
  type t =
    { view : Vdom.Node.t
    ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
    }

  module Graph : Graph

  module Selector : sig
    type t =
      | Flame of Graph.Tree.t
      | Icicle of Graph.Tree.t
      | Focus of Graph.Sequence.t

    val same : t -> t -> bool
  end

  val component
    :  Graph.t Bonsai.Value.t
    -> selection:Selector.t option Bonsai.Value.t
    -> select:(Selector.t -> unit Vdom.Effect.t) Bonsai.Value.t
    -> navigate_to:(Selector.t -> unit Vdom.Effect.t) Bonsai.Value.t
    -> activate:(Selector.t -> unit Vdom.Effect.t) Bonsai.Value.t
    -> commands:Commands.t Bonsai.Value.t
    -> t Bonsai.Computation.t

  val scroll_selection_into_view : unit Effect.t
  val scroll_focus_into_view : unit Effect.t
end

module type Flame_graph_view = sig
  module Node_type = Node_type

  module type Graph = Graph

  module Direction = Direction
  module Command = Command
  module Commands = Commands

  module type S = S

  module Make (Graph : Graph) : S with module Graph := Graph
end
