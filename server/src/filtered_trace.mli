open! Core
open Memtrace_viewer_common

module Event : sig
  type t = Location.t Event.t [@@deriving sexp_of]
end

type t

val create : trace:Raw_trace.t -> loc_cache:Location.Cache.t -> filter:Filter.t -> t
val word_size : t -> Byte_units.t
val sample_rate : t -> float

module Mode : sig
  type t =
    | Preserve_backtraces
    (** Events will have backtraces but may not be in the right order and may not have
        accurate timestamps *)
    | Preserve_times
    (** Events will come in the correct order, with accurate times, but have empty
        backtraces *)
end

val iter : t -> mode:Mode.t -> (Time_ns.Span.t -> Event.t -> unit) -> unit

module Call_sites : sig
  module Callees_from_call_site : sig
    type t = Location.Hash_set.t
  end

  module Calls_from_location : sig
    type t = Callees_from_call_site.t Call_site.Table.t
  end

  type t = Calls_from_location.t Location.Table.t
end

val iter_and_gather_call_sites
  :  t
  -> mode:Mode.t
  -> (Time_ns.Span.t -> Event.t -> unit)
  -> Call_sites.t
