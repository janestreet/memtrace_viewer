open! Core_kernel
open Bonsai_web

(* [Bonsai_simple_table] provides a simplistic table widget. It can lay things out in a
   <table> tag and has basic keyboard support for focusing a row.

   And that's about it! In particular, there is no support for:

   - Focusing a cell within a row
   - Searching
   - Dynamic sorting
   - Any kind of partial render -- i.e. this will not scale to many many rows
*)

module Col_group : String_id.S =
  String_id.Make
    (struct
      let module_name = "Bonsai_simple_table.Col_group"
    end)
    ()

module type Id = sig
  type t [@@deriving equal, sexp]

  include Comparable.S with type t := t
end

module type Row = sig
  type t

  module Id : Id
end

module type S = sig
  module Row : Row
  module Col_id : Id

  (** This will be turned into a [td] with contents [node] and attributes [attrs]. *)
  type cell =
    { node : Vdom.Node.t
    ; attrs : Vdom.Attr.t list
    }

  module Column : sig
    type t

    (** Create a column specification.

        [group] allows grouping of columns. A run of adjacent (as defined by [index])
        columns that share the same [group] will have their headers displayed in a two-row
        layout, like:

        {v
         | group name
         +---------+---------+
         | header1 | header2 | ...
         +---------+---------+
         | ...     | ...     | ...
        v}
    *)
    val create
      :  ?header_for_testing:string
      -> ?classes:string list
      -> header:cell
      -> render:(Row.Id.t -> Row.t -> cell)
      -> group:Col_group.t option
      -> unit
      -> t
  end

  module Model : sig
    type t [@@deriving sexp_of]

    val create : unit -> t
  end

  module Input : sig
    (** In addition to specifying the full range of rows and cols, we also ask for the IDs
        in order. This can be used to implement both filtering and sorting (although there
        is no support within this component to e.g. sort on a column when the user clicks
        on the header). *)

    type t =
      { rows : Row.t Row.Id.Map.t
      ; cols : Column.t Col_id.Map.t
      ; row_ids_in_order : [ `All_in_default_order | `These of Row.Id.t list ]
      ; col_ids_in_order : Col_id.t list
      ; table_attrs : Vdom.Attr.t list
      }
  end

  module Action : sig
    type t =
      | Set_focus_row of Row.Id.t option
      | Move_focus of [ `Prev | `Next ]
    [@@deriving sexp_of]
  end

  module Result : sig
    type t =
      { view : Vdom.Node.t
      ; view_for_testing : string Lazy.t
      ; key_handler : Vdom_keyboard.Keyboard_event_handler.t
      ; focus_row : (Row.Id.t * Row.t) option
      ; inject : Action.t -> Vdom.Event.t
      }
  end

  val bonsai : (Input.t, Result.t) Bonsai.Arrow.t
end

module type Bonsai_simple_table = sig
  module Col_group = Col_group

  module type Id = Id
  module type Row = Row
  module type S = S

  module Make (Row : Row) (Col_id : Id) :
    S with module Row := Row and module Col_id := Col_id
end
