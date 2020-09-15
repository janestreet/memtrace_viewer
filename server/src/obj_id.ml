open! Core_kernel
open Memtrace

module T = struct
  type t = Trace.Obj_id.t

  let hash t = Int.hash (t : t :> int)
  let hash_fold_t s t = Int.hash_fold_t s (t : t :> int)
  let compare t1 t2 = Int.compare (t1 : t :> int) (t2 : t :> int)
  let sexp_of_t t = Int.sexp_of_t (t : t :> int)
end

include T
include Hashable.Make_plain (T)
