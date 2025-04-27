open! Core

module String_relation = struct
  type t =
    | Equals
    | Contains
  [@@deriving sexp, bin_io, equal]
end

module Location_predicate = struct
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

let always_true =
  { allocated_range = Range.Time_ns_span.all
  ; collected_range = Non_empty Range.Time_ns_span.all
  ; size_range = Range.Byte_units.all
  ; lifetime_range = Range.Time_ns_span.all
  ; required_locations = []
  ; forbidden_locations = []
  ; hidden_locations = []
  ; include_minor_heap = true
  ; include_major_heap = true
  ; include_external = true
  }
;;

let is_always_true = function
  | { allocated_range
    ; collected_range
    ; size_range
    ; lifetime_range
    ; required_locations
    ; forbidden_locations
    ; hidden_locations
    ; include_minor_heap
    ; include_major_heap
    ; include_external
    } ->
    Range.Time_ns_span.is_all allocated_range
    && Range.Time_ns_span.Or_empty.is_all collected_range
    && Range.Byte_units.is_all size_range
    && Range.Time_ns_span.is_all lifetime_range
    && List.is_empty required_locations
    && List.is_empty forbidden_locations
    && List.is_empty hidden_locations
    && include_minor_heap
    && include_major_heap
    && include_external
;;
