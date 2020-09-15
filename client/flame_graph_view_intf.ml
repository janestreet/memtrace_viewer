open! Core_kernel
open Bonsai_web

module type Tree = sig
  module Handle : sig
    type t [@@deriving sexp, equal]
  end

  module Node : sig
    type t

    val handle : t -> Handle.t
    val label : t -> string
    val details : t -> string
    val size : t -> float
    val children : t -> t list
  end

  type t

  val roots : t -> Node.t list
  val find : t -> Handle.t -> Node.t option

  (** Return all the nodes leading to the one with the given handle, starting with its
      root and ending with the node itself. *)
  val find_ancestry : t -> Handle.t -> Node.t list option

  val find_parent : t -> Node.t -> Node.t option
  val is_related : t -> strictly:bool -> ancestor:Node.t -> descendant:Node.t -> bool
end

module type S = sig
  module type Tree = Tree

  module Shape : sig
    type t =
      | Flames
      | Icicles
  end

  module Make (Tree : Tree) : sig
    type t

    val view : t -> Vdom.Node.t
    val focus : t -> Tree.Handle.t option
    val key_handler : t -> Vdom_keyboard.Keyboard_event_handler.t

    val component
      :  tree:Tree.t Bonsai.Value.t
      -> shape:Shape.t Bonsai.Value.t
      -> width:int Bonsai.Value.t
      -> zoom:Tree.Handle.t option Bonsai.Value.t
      -> t Bonsai.Computation.t
  end
end
