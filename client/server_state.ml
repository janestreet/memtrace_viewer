open! Core_kernel

module Status = struct
  type t =
    | Up
    | Down

  let is_up = function
    | Up -> true
    | Down -> false
  ;;
end

type t = { status : Status.t }

let initial = { status = Up }
