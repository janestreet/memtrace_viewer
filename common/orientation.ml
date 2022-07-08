type t =
  | Callers
  | Callees
[@@deriving sexp, equal]

let flip = function
  | Callers -> Callees
  | Callees -> Callers
;;
