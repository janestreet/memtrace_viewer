open! Core_kernel

type t =
  { lower_bound : Time_ns.Span.t option
  ; upper_bound : Time_ns.Span.t option
  }
[@@deriving sexp, bin_io, equal]

let range lower_bound upper_bound = { lower_bound; upper_bound }
let all = { lower_bound = None; upper_bound = None }

let is_all = function
  | { lower_bound = None; upper_bound = None } -> true
  | _ -> false
;;

let covers { lower_bound; upper_bound } ~lower ~upper =
  let covers_lower =
    match lower_bound with
    | None -> true
    | Some lower_bound -> Time_ns.Span.(lower_bound <= lower)
  in
  let covers_upper =
    match upper_bound with
    | None -> true
    | Some upper_bound -> Time_ns.Span.(upper_bound >= upper)
  in
  covers_lower && covers_upper
;;

let compare_point x { lower_bound; upper_bound } =
  let in_bound f x bound =
    match bound with
    | None -> true
    | Some bound -> f x bound
  in
  match x with
  | x when not (in_bound Time_ns.Span.( >= ) x lower_bound) -> -1
  | x when not (in_bound Time_ns.Span.( <= ) x upper_bound) -> 1
  | _ -> 0
;;
