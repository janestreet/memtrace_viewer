open! Core

module Status = struct
  type t =
    | Idle
    | Busy
    | Down

  let is_idle = function
    | Idle -> true
    | Busy | Down -> false
  ;;
end

type t = { status : Status.t }

let initial = { status = Idle }
