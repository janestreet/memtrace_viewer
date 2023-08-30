type t =
  | Callers
  | Callees
[@@deriving sexp, equal]

val flip : t -> t
