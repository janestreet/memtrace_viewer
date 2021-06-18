open! Core
include Identifier_intf

module Make () : S_with_special = struct
  module T = struct
    type t = int [@@deriving equal, compare, hash, sexp, bin_io]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let max_value = Int.max_value
  let first_special = -1
  let next_special t = t - 1
  let is_special t = t < 0

  module Generator = struct
    type t = int ref [@@deriving sexp]

    let create () = ref 0

    let generate t =
      let id = !t in
      incr t;
      id
    ;;
  end
end
