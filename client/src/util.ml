open! Core
open Bonsai_web_proc

module Float_html_syntax : Stringable.S with type t = float = struct
  type t = float

  let to_string x = sprintf "%g" x
  let of_string = Float.of_string
end

let placeholder_span = Vdom.Node.span []
let placeholder_div = Vdom.Node.div []
let placeholder_svg = Virtual_dom_svg.Node.g []

module Option_model (T : sig
    type t [@@deriving sexp_of]
  end) =
struct
  type t = T.t option [@@deriving sexp_of]
end

let overridden_local_zone = ref None

let local_zone () =
  match !overridden_local_zone with
  | Some zone -> zone
  | None -> force Timezone.local
;;

let override_local_zone zone = overridden_local_zone := Some zone

let abbreviation_via_core ~zone time =
  Time_float.Zone.abbreviation zone (Time_ns.to_time_float_round_nearest time)
;;

let get_timezone_abbrev date =
  let open Js_of_ocaml in
  let options = Intl.DateTimeFormat.options () in
  options##.timeZoneName := Js.def (Js.string "short");
  let formatter = new%js Intl.dateTimeFormat_constr Js.undefined (Js.def options) in
  let formatted = formatter##.format date |> Js.to_string in
  (* The format will be like "7/3/2025, EST" - extract the timezone part after the last
     space *)
  match String.rsplit2 formatted ~on:' ' with
  | Some (_, tz) -> Some tz
  | None -> None
;;

let local_zone_abbreviation_from_intl time =
  let open Js_of_ocaml in
  match Intl.is_supported () with
  | false -> None
  | true ->
    let date =
      let ms = Time_ns.to_span_since_epoch time |> Time_ns.Span.to_ms_approx in
      new%js Js.date_fromTimeValue (Js.number_of_float ms)
    in
    get_timezone_abbrev date
;;

let local_zone_abbreviation time =
  match !overridden_local_zone with
  | Some zone -> abbreviation_via_core ~zone time
  | None ->
    (match local_zone_abbreviation_from_intl time with
     | None -> abbreviation_via_core ~zone:(local_zone ()) time
     | Some abbreviation -> abbreviation)
;;
