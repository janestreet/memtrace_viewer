open! Core
open Bonsai_web_proc

(* An implementation of [Stringable.S] for [float] that is compatible with HTML/JavaScript
   float syntax. The usual [Float.to_string] is prone to outputting things like "1." that
   only make sense to OCaml. *)
module Float_html_syntax : Stringable.S with type t = float

(* An empty span meant as a placeholder for an element that might exist later. Helps with
   diffs because whenever a new element is inserted, its rightward siblings all get their
   DOM trees rebuilt from scratch. *)
val placeholder_span : Vdom.Node.t

(* An empty div meant as a placeholder. See [placeholder_span]. *)
val placeholder_div : Vdom.Node.t

(* An empty SVG node meant as a placeholder. See [placeholder_svg]. *)
val placeholder_svg : Vdom.Node.t

(* Wrap a type suitable for use as a Bonsai model in the option type. *)
module Option_model (T : sig
    type t [@@deriving sexp_of]
  end) : sig
  type t = T.t option [@@deriving sexp_of]
end

(** Returns the local zone used by wall-time controls. Defaults to [Timezone.local], but
    can be changed with [override_local_zone]. *)
val local_zone : unit -> Time_float.Zone.t

(** Overrides the local zone used by [local_zone] and [local_zone_abbreviation]. This is
    intended for tests. *)
val override_local_zone : Time_float.Zone.t -> unit

(** Best-effort short name (e.g. "EDT", "BST") for the local time zone at the given time.

    When [override_local_zone] has been called, this uses [Time_float.Zone.abbreviation]
    on the overridden zone. Otherwise, it uses the browser's [Intl.DateTimeFormat]
    [timeZoneName: "short"] formatter when available, falling back to
    [Time_float.Zone.abbreviation] applied to [Timezone.local]. *)
val local_zone_abbreviation : Time_ns.t -> string
