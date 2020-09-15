(* Based on http://github.com/rm-hull/loose-labels, which is based on "Nice numbers for
   graph labels" in Graphics Gems, vol. 1, modified to generate labels *inside* the given
   bounds rather than outside them *)

open! Core_kernel

module type Diff = sig
  type t

  val round_to_nice : dir:[ `Up | `Near | `Down ] -> t -> t
  val ( / ) : t -> float -> t
end

module type S = sig
  type t

  module Diff : Diff

  val ( + ) : t -> Diff.t -> t
  val ( - ) : t -> t -> Diff.t
  val round_to_multiple_of_nice : relative_to:t -> dir:[ `Up | `Down ] -> t -> Diff.t -> t
  val ( = ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
end

let nice_num x ~round_dir =
  assert (Float.(x >= 0.));
  let exp = Float.round_down (Float.log10 x) in
  let d = 10. ** exp in
  let f = if Float.(d = 0.) then x else x /. d in
  let nf =
    match round_dir with
    | `Near ->
      (match () with
       | _ when Float.(f < 1.5) -> 1.
       | _ when Float.(f < 3.) -> 2.
       | _ when Float.(f < 7.) -> 5.
       | _ -> 10.)
    | `Up ->
      (match () with
       | _ when Float.(f <= 1.) -> 1.
       | _ when Float.(f <= 2.) -> 2.
       | _ when Float.(f <= 5.) -> 5.
       | _ -> 10.)
    | `Down ->
      (match () with
       | _ when Float.(f < 1.) -> 0.5
       | _ when Float.(f < 2.) -> 1.
       | _ when Float.(f < 5.) -> 2.
       | _ -> 5.)
  in
  nf *. (10. ** exp)
;;

let nice_num_out_of_24 ~round_dir f =
  assert (Float.(f >= 1. && f < 24.));
  match round_dir with
  | `Near ->
    (match () with
     | _ when Float.(f < 1.5) -> 1.
     | _ when Float.(f < 3.5) -> 2.
     | _ when Float.(f < 8.5) -> 6.
     | _ when Float.(f < 17.) -> 12.
     | _ -> 24.)
  | `Up ->
    (match () with
     | _ when Float.(f <= 1.) -> 1.
     | _ when Float.(f <= 2.) -> 2.
     | _ when Float.(f <= 6.) -> 6.
     | _ when Float.(f <= 12.) -> 12.
     | _ -> 24.)
  | `Down ->
    (match () with
     | _ when Float.(f < 2.) -> 1.
     | _ when Float.(f < 6.) -> 2.
     | _ when Float.(f < 12.) -> 6.
     | _ -> 12.)
;;

let nice_num_out_of_60 ~round_dir f =
  assert (Float.(f >= 1. && f < 60.));
  match round_dir with
  | `Near ->
    (match () with
     | _ when Float.(f < 1.5) -> 1.
     | _ when Float.(f < 3.) -> 2.
     | _ when Float.(f < 7.) -> 5.
     | _ when Float.(f < 12.5) -> 10.
     | _ when Float.(f < 22.5) -> 15.
     | _ when Float.(f < 45.) -> 30.
     | _ -> 60.)
  | `Up ->
    (match () with
     | _ when Float.(f <= 1.) -> 1.
     | _ when Float.(f <= 2.) -> 2.
     | _ when Float.(f <= 5.) -> 5.
     | _ when Float.(f <= 10.) -> 10.
     | _ when Float.(f <= 15.) -> 15.
     | _ when Float.(f <= 30.) -> 30.
     | _ -> 60.)
  | `Down ->
    (match () with
     | _ when Float.(f < 2.) -> 1.
     | _ when Float.(f < 5.) -> 2.
     | _ when Float.(f < 10.) -> 5.
     | _ when Float.(f < 15.) -> 10.
     | _ when Float.(f < 30.) -> 15.
     | _ -> 30.)
;;

module S_float : S with type t = float and type Diff.t = float = struct
  module Diff = struct
    type t = float

    let round_to_nice ~dir x = nice_num x ~round_dir:dir
    let ( / ) = Float.( / )
  end

  include Float

  let round_to_multiple_of_nice ~relative_to ~dir x d =
    let round =
      match dir with
      | `Down -> Float.round_down
      | `Up -> Float.round_up
    in
    (round ((x -. relative_to) /. d) *. d) +. relative_to
  ;;
end

module Diff_time_ns_span : Diff with type t = Time_ns.Span.t = struct
  type t = Time_ns.Span.t

  let ( / ) t x = Time_ns.Span.(t / x)

  let round_to_nice ~dir t =
    assert (Time_ns.Span.is_positive t);
    match Time_ns.Span.to_parts t with
    | { hr; _ } when hr >= 24 ->
      nice_num ~round_dir:dir ((hr |> Float.of_int) /. 24.) |> Time_ns.Span.of_day
    | { hr; _ } when hr > 0 ->
      nice_num_out_of_24 ~round_dir:dir (hr |> Float.of_int) |> Time_ns.Span.of_hr
    | { min; _ } when min > 0 ->
      nice_num_out_of_60 ~round_dir:dir (t |> Time_ns.Span.to_min) |> Time_ns.Span.of_min
    | { sec; _ } when sec > 0 ->
      nice_num_out_of_60 ~round_dir:dir (t |> Time_ns.Span.to_sec) |> Time_ns.Span.of_sec
    | _ -> nice_num ~round_dir:dir (t |> Time_ns.Span.to_ns) |> Time_ns.Span.of_ns
  ;;
end

module S_time_ns_span : S with type t = Time_ns.Span.t and type Diff.t = Time_ns.Span.t =
struct
  module Diff = Diff_time_ns_span
  include Time_ns.Span

  let round_to_multiple_of_nice ~relative_to ~dir t d =
    let floor =
      Time_ns.Span.(scale_int63 d (div (t + neg relative_to) d) + relative_to)
    in
    match dir with
    | `Up when Time_ns.Span.(floor < t) -> floor + d
    | _ -> floor
  ;;
end

module S_time_ns : S with type t = Time_ns.t and type Diff.t = Time_ns.Span.t = struct
  module Diff = Diff_time_ns_span

  type t = Time_ns.t

  let ( + ) = Time_ns.add
  let ( - ) = Time_ns.diff
  let ( <= ) = Time_ns.( <= )
  let ( = ) = Time_ns.( = )

  let round_to_multiple_of_nice ~relative_to ~dir t d =
    let diff = Time_ns.diff t relative_to in
    let diff =
      S_time_ns_span.round_to_multiple_of_nice ~relative_to:Time_ns.Span.zero ~dir diff d
    in
    Time_ns.add relative_to diff
  ;;
end

module Make (T : S) = struct
  let round x = T.Diff.round_to_nice ~dir:`Near x
  let round_down x = T.Diff.round_to_nice ~dir:`Down x
  let round_up x = T.Diff.round_to_nice ~dir:`Up x

  let round_down_to_multiple_of_nice ~relative_to ~step x =
    T.round_to_multiple_of_nice ~relative_to ~dir:`Down x step
  ;;

  let round_up_to_multiple_of_nice ~relative_to ~step x =
    T.round_to_multiple_of_nice ~relative_to ~dir:`Up x step
  ;;

  let loose_labels ?(max_count = 5) ~relative_to lower upper =
    assert (max_count >= 2 && T.(lower <= upper));
    if T.(lower = upper)
    then []
    else (
      let range = T.Diff.round_to_nice ~dir:`Near T.(upper - lower) in
      let d =
        T.Diff.round_to_nice ~dir:`Up T.Diff.(range / (max_count - 1 |> Float.of_int))
      in
      let graph_min = T.round_to_multiple_of_nice ~relative_to ~dir:`Down lower d in
      let graph_max = T.round_to_multiple_of_nice ~relative_to ~dir:`Up upper d in
      let rec range start end_ step =
        if T.(end_ <= start) then [] else start :: range T.(start + step) end_ step
      in
      let start = if T.(graph_min = lower) then graph_min else T.(graph_min + d) in
      let end_ =
        if T.(graph_max = upper)
        then T.(graph_max + T.Diff.(d / 2.))
        else T.(graph_max + T.Diff.(d / -2.))
      in
      range start end_ d)
  ;;
end

include Make (S_float)

let loose_labels = loose_labels ~relative_to:0.

module Time_ns = struct
  include Make (S_time_ns)

  let start_of_day_utc t =
    let zone = Time.Zone.utc in
    let date = t |> Time_ns.to_date ~zone in
    Time_ns.of_date_ofday ~zone date Time_ns.Ofday.start_of_day
  ;;

  module Span = struct
    include Make (S_time_ns_span)

    let loose_labels = loose_labels ~relative_to:Time_ns.Span.zero
  end
end
