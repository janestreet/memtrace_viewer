open! Core_kernel
open Bonsai_web
open Memtrace_viewer_common

type t =
  { view : Vdom.Node.t
  ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
  }

module Orientations : sig
  type t =
    | (* The double flame graph is visible (i.e., the focus isn't an endpoint) *)
      Both
    | (* Either a single flame/icicle graph is visible (i.e., the focus is an endpoint)
         or one of the tables is *)
      Only of
        Data.Orientation.t
end

module Which_interface : sig
  type t =
    | Flame_graph
    | Table
end

val component
  :  trie:Data.Fragment_trie.t Bonsai.Value.t
  -> orients_visible:Orientations.t Bonsai.Value.t
  -> which_interface:Which_interface.t Bonsai.Value.t
  -> focus:Data.Fragment.Oriented.t option Bonsai.Value.t
  -> poi:Data.Fragment.t Bonsai.Value.t
  -> set_poi:(Data.Fragment.t -> Vdom.Event.t) Bonsai.Value.t
  -> zoom:Data.Fragment.t Bonsai.Value.t
  -> set_zoom:(Data.Fragment.t option -> Vdom.Event.t) Bonsai.Value.t
  -> total_allocations:Byte_units.t Bonsai.Value.t
  -> t Bonsai.Computation.t
