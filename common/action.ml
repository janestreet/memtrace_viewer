open! Core_kernel

type t = Set_filter of Filter.t [@@deriving sexp, bin_io]
