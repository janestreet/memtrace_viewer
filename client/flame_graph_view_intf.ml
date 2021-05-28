open! Core_kernel
open Bonsai_web

module Shape = struct
  type t =
    | Flames
    | Icicles
  [@@deriving sexp, equal]
end

module type Tree = sig
  module Node : sig
    type t

    val label : shape:Shape.t -> t -> string
    val details : shape:Shape.t -> t -> string
    val size : t -> float
    val hidden : shape:Shape.t -> t -> bool
    val children : shape:Shape.t -> t -> t list
    val parent : shape:Shape.t -> t -> t option
    val same : t -> t -> bool

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  type t

  val root : t -> Node.t

  val is_related
    :  t
    -> shape:Shape.t
    -> strictly:bool
    -> ancestor:Node.t
    -> descendant:Node.t
    -> bool
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

  module Tree : Tree

  module Node : sig
    type t =
      { tree_node : Tree.Node.t
      ; shape : Shape.t
      }

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end

  val component
    :  tree:Tree.t Bonsai.Value.t
    -> width:int Bonsai.Value.t
    -> focus:Node.t option Bonsai.Value.t
    -> set_focus:(Node.t option -> Vdom.Event.t) Bonsai.Value.t
    -> zoom:Tree.Node.t Bonsai.Value.t
    -> set_zoom:(Tree.Node.t option -> Vdom.Event.t) Bonsai.Value.t
    -> t Bonsai.Computation.t
end

module type Flame_graph_view = sig
  module type Tree = Tree

  module Shape = Shape
  module Direction = Direction

  module type S = S

  module Make (Tree : Tree) : S with module Tree := Tree
end
