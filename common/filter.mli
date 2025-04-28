open! Core

module String_relation : sig
  type t =
    | Equals
    | Contains
  [@@deriving sexp, bin_io, equal]
end

module Location_predicate : sig
  type t =
    | Defname_related of
        { relation : String_relation.t
        ; rhs : string
        }
  [@@deriving sexp, bin_io, equal]
end

type t =
  { allocated_range : Range.Time_ns_span.t
  ; collected_range : Range.Time_ns_span.Or_empty.t
  ; size_range : Range.Byte_units.t
  ; lifetime_range : Range.Time_ns_span.t
  ; required_locations : Location_predicate.t list
  ; forbidden_locations : Location_predicate.t list
  ; hidden_locations : Location_predicate.t list
  ; include_minor_heap : bool
  ; include_major_heap : bool
  ; include_external : bool
  }
[@@deriving sexp, bin_io, equal]

val always_true : t
val is_always_true : t -> bool
