open! Core
open Bonsai_web

module type Graph = sig
  type t
  type graph := t

  module Node : sig
    type t

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
    -> width:int Bonsai.Value.t
    -> selection:Selector.t option Bonsai.Value.t
    -> select:(Selector.t -> Vdom.Event.t) Bonsai.Value.t
    -> navigate_to:(Selector.t -> Vdom.Event.t) Bonsai.Value.t
    -> activate:(Selector.t -> Vdom.Event.t) Bonsai.Value.t
    -> t Bonsai.Computation.t
end

module type Flame_graph_view = sig
  module type Graph = Graph

  module Direction = Direction

  module type S = S

  module Make (Graph : Graph) : S with module Graph := Graph
end
