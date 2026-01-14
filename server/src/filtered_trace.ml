open! Core
open Memtrace_viewer_common

let predicate_matches ~loc_cache (pred : Filter.Location_predicate.t) call_site =
  let data = Location.Cache.get_call_site_data loc_cache call_site in
  let defname = Data.Call_site.defname data in
  match pred with
  | Defname_related { relation; rhs } ->
    (match relation with
     | Equals -> String.equal defname rhs
     | Contains -> String.is_substring defname ~substring:rhs)
;;

module Filtered_location_cache = struct
  type t =
    | Trivial of Location.Cache.t
    | Nontrivial of
        { loc_cache : Location.Cache.t
        ; hidden_locations : Filter.Location_predicate.t list
        ; cache : Call_site.t list Location.Code.Table.t
        }

  let create ~filter:({ hidden_locations; _ } : Filter.t) ~loc_cache () =
    match hidden_locations with
    | [] -> Trivial loc_cache
    | _ ->
      Nontrivial { loc_cache; hidden_locations; cache = Location.Code.Table.create () }
  ;;

  let loc_cache (Trivial loc_cache | Nontrivial { loc_cache; _ }) = loc_cache

  let call_sites_from_code t code =
    match t with
    | Trivial loc_cache -> Location.Cache.call_sites_from_code loc_cache code
    | Nontrivial { loc_cache; hidden_locations; cache } ->
      Hashtbl.find_or_add cache code ~default:(fun () ->
        let call_sites = Location.Cache.call_sites_from_code loc_cache code in
        List.filter call_sites ~f:(fun call_site ->
          not
            (List.exists hidden_locations ~f:(fun pred ->
               predicate_matches ~loc_cache pred call_site))))
  ;;

  let last_call_site_is_hidden t code =
    match t with
    | Trivial _ -> false
    | Nontrivial { loc_cache; _ } ->
      let filtered_call_sites = call_sites_from_code t code in
      let all_call_sites = Location.Cache.call_sites_from_code loc_cache code in
      not
        (Option.equal
           Call_site.equal
           (List.last filtered_call_sites)
           (List.last all_call_sites))
  ;;

  let get_function_of_call_site t call_site =
    Location.Cache.get_function_of_call_site (loc_cache t) call_site
  ;;

  let get_allocation_site_of_call_site t call_site =
    Location.Cache.get_allocation_site_of_call_site (loc_cache t) call_site
  ;;
end

module Cached_predicate : sig
  type t

  val wrap : loc_cache:Location.Cache.t -> Filter.Location_predicate.t -> t
  val matches : t -> Call_site.t -> bool
end = struct
  type t =
    { pred : Filter.Location_predicate.t
    ; cache : bool Call_site.Table.t
    ; loc_cache : Location.Cache.t
    }

  let wrap ~loc_cache pred = { pred; cache = Call_site.Table.create (); loc_cache }

  let matches { pred; cache; loc_cache } call_site =
    Hashtbl.find_or_add cache call_site ~default:(fun () ->
      predicate_matches ~loc_cache pred call_site)
  ;;
end

(** Is an object allocated at this time eligible to pass the filter? *)
let should_record_allocation_at time ({ allocated_range; _ } : Filter.t) =
  Range.Time_ns_span.compare_point time allocated_range = 0
;;

(** Is an allocation with the given size eligible to pass the filter? *)
let should_record_allocation_of_size size ({ size_range; _ } : Filter.t) =
  Range.Byte_units.compare_point size size_range = 0
;;

(** Is an object with the given lifetime eligible to pass the filter? *)
let should_keep_object_with_lifetime lifetime ({ lifetime_range; _ } : Filter.t) =
  Range.Time_ns_span.compare_point lifetime lifetime_range = 0
;;

module Location_filterer : sig
  type t

  val create : filter:Filter.t -> loc_cache:Location.Cache.t -> unit -> t
  val always_passes : t -> bool
  val should_record_allocation_with_backtrace : t -> Location.Code.t array -> int -> bool
end = struct
  type t =
    { required_locations : Cached_predicate.t list
    ; forbidden_locations : Cached_predicate.t list
    ; loc_cache : Location.Cache.t
    ; always_passes : bool
    }

  let create ~(filter : Filter.t) ~loc_cache () =
    let always_passes =
      match filter with
      | { required_locations = []
        ; forbidden_locations = []
        ; hidden_locations = _
        ; allocated_range = _
        ; collected_range = _
        ; size_range = _
        ; lifetime_range = _
        ; include_major_heap = _
        ; include_minor_heap = _
        ; include_external = _
        } -> true
      | _ -> false
    in
    let required_locations =
      List.map ~f:(Cached_predicate.wrap ~loc_cache) filter.required_locations
    in
    let forbidden_locations =
      List.map ~f:(Cached_predicate.wrap ~loc_cache) filter.forbidden_locations
    in
    { required_locations; forbidden_locations; loc_cache; always_passes }
  ;;

  let always_passes t = t.always_passes

  (** Is an allocation with the given backtrace eligible to pass the filter? *)
  let should_record_allocation_with_backtrace t buffer length =
    let holds_somewhere pred =
      let rec holds_somewhere_from i =
        if i >= length
        then false
        else (
          let call_sites = Location.Cache.call_sites_from_code t.loc_cache buffer.(i) in
          List.exists ~f:(Cached_predicate.matches pred) call_sites
          || holds_somewhere_from (i + 1))
      in
      holds_somewhere_from 0
    in
    let interesting = List.for_all ~f:holds_somewhere t.required_locations in
    let forbidden () = List.exists ~f:holds_somewhere t.forbidden_locations in
    interesting && not (forbidden ())
  ;;
end

(** Is an object collected at this time eligible to pass the filter? *)
let should_keep_object_collected_at time ({ collected_range; _ } : Filter.t) =
  Range.Time_ns_span.Or_empty.contains_point collected_range time
;;

(** Are objects that are never collected eligible to pass the filter? (This counts objects
    that are live at the end as collected at the end, unless the filter specifies "never
    collected". In other words, "collected at or after" includes "collected never". This
    is important because "live at t" is interpreted as "allocated at or before t and
    collected after t". *)
let should_keep_objects_that_are_never_collected
  ~time_at_end
  ({ collected_range; _ } : Filter.t)
  =
  Range.Time_ns_span.Or_empty.(
    is_empty collected_range || contains_point collected_range time_at_end)
;;

let obj_ids_matching_filter ~trace ~loc_cache (filter : Filter.t) =
  assert (
    filter.include_minor_heap || filter.include_major_heap || filter.include_external);
  (* Objects that are live in an interesting heap *)
  let live = Obj_id.Table.create () in
  (* Objects that will become live if promoted *)
  let prelive = Obj_id.Hash_set.create () in
  let passing = Obj_id.Hash_set.create () in
  let location_filterer = Location_filterer.create ~filter ~loc_cache () in
  let filtering_by_backtrace = not (Location_filterer.always_passes location_filterer) in
  let parse_backtraces = filtering_by_backtrace in
  (* If true, we should accept minor allocations but defer them rather than counting them
     as allocated right away *)
  let defer_minor_allocations =
    filter.include_major_heap && not filter.include_minor_heap
  in
  Raw_trace.iter ~parse_backtraces trace (fun time event ->
    let defer obj_id = Hash_set.strict_add_exn prelive obj_id in
    let is_deferred obj_id = Hash_set.mem prelive obj_id in
    let allocate obj_id =
      Hash_set.remove prelive obj_id;
      if should_record_allocation_at time filter
      then Hashtbl.add_exn live ~key:obj_id ~data:time
    in
    let deallocate obj_id =
      if Hash_set.mem prelive obj_id
      then (
        Hash_set.strict_remove_exn prelive obj_id;
        assert (not (Hashtbl.mem live obj_id)))
      else (
        match Hashtbl.find live obj_id with
        | None -> ()
        | Some alloc_time ->
          Hashtbl.remove live obj_id;
          let lifetime = Time_ns.Span.( - ) time alloc_time in
          if should_keep_object_collected_at time filter
             && should_keep_object_with_lifetime lifetime filter
          then Hash_set.strict_add_exn passing obj_id)
    in
    match event with
    | Alloc
        { obj_id; single_allocation_size; source; backtrace_length; backtrace_buffer; _ }
      ->
      let correct_heap, action_on_pass =
        match source with
        | Minor when defer_minor_allocations -> true, `Defer
        | Minor -> filter.include_minor_heap, `Allocate
        | Major -> filter.include_major_heap, `Allocate
        | External -> filter.include_external, `Allocate
      in
      let correct_size = should_record_allocation_of_size single_allocation_size filter in
      let interesting_backtrace () =
        (not filtering_by_backtrace)
        || Location_filterer.should_record_allocation_with_backtrace
             location_filterer
             backtrace_buffer
             backtrace_length
      in
      let pass = correct_heap && correct_size && interesting_backtrace () in
      (match pass with
       | false -> ()
       | true ->
         (match action_on_pass with
          | `Defer -> defer obj_id
          | `Allocate -> allocate obj_id))
    | Promote obj_id ->
      if is_deferred obj_id
      then allocate obj_id
      else if not filter.include_major_heap
      then deallocate obj_id
    | Collect obj_id ->
      (* Might already have been filtered out, but [deallocate] is harmless then *)
      deallocate obj_id
    | End ->
      let time_at_end = time in
      if should_keep_objects_that_are_never_collected filter ~time_at_end
      then
        Hashtbl.iteri live ~f:(fun ~key:obj_id ~data:alloc_time ->
          let lifetime = Time_ns.Span.( - ) time_at_end alloc_time in
          if should_keep_object_with_lifetime lifetime filter
          then Hash_set.strict_add_exn passing obj_id));
  passing
;;

type t =
  { trace : Raw_trace.t
  ; loc_cache : Filtered_location_cache.t
  ; interesting : Obj_id.t -> bool
  ; defer_minor_allocations : bool
  ; collect_on_promotion : bool
  }

let word_size t = (Raw_trace.info t.trace).word_size
let sample_rate t = (Raw_trace.info t.trace).sample_rate

let create ~trace ~loc_cache ~filter =
  let interesting =
    if Filter.is_always_true filter
    then fun _ -> true
    else (
      let interesting = obj_ids_matching_filter ~trace ~loc_cache filter in
      fun obj_id -> Hash_set.mem interesting obj_id)
  in
  let filtered_loc_cache = Filtered_location_cache.create ~filter ~loc_cache () in
  let defer_minor_allocations =
    filter.include_major_heap && not filter.include_minor_heap
  in
  let collect_on_promotion = not filter.include_major_heap in
  { trace
  ; loc_cache = filtered_loc_cache
  ; interesting
  ; defer_minor_allocations
  ; collect_on_promotion
  }
;;

module Event = struct
  type t = Location.t Event.t [@@deriving sexp_of]
end

module Call_sites = struct
  module Callees_from_call_site = struct
    (* The set of locations that a particular call site invokes. (There can be multiple
       locations only if it's an indirect call.) *)
    type t = Location.Hash_set.t
  end

  module Calls_from_location = struct
    (* The [Callees_from_call_site.t] for each call site within a particular location. *)
    type t = Callees_from_call_site.t Call_site.Table.t
  end

  type t = Calls_from_location.t Location.Table.t

  let of_list (l : (Location.t * Call_site.t * Location.t list) list) =
    let t = Location.Table.create () in
    List.iter l ~f:(fun (loc, call_site, callees) ->
      let calls_from_location =
        Hashtbl.find_or_add t loc ~default:Call_site.Table.create
      in
      Hashtbl.add_exn
        calls_from_location
        ~key:call_site
        ~data:(callees |> Location.Hash_set.of_list));
    t
  ;;
end

module Mode = struct
  type t =
    | Preserve_backtraces
    | Preserve_times
end

module Interpreter : sig
  type filtered_trace := t
  type t

  val create
    :  filtered_trace:filtered_trace
    -> callback:(Time_ns.Span.t -> Event.t -> unit)
    -> mode:Mode.t
    -> record_call_sites:bool
    -> unit
    -> t

  val interpret_event : t -> Time_ns.Span.t -> Raw_trace.Event.t -> unit
  val call_sites : t -> Call_sites.t
end = struct
  module Location_state = struct
    type t =
      { location : Location.t
      ; mutable seen : bool
      }

    let dummy = { location = Location.dummy; seen = false }
  end

  module Call_site_state = struct
    type t =
      { call_site : Call_site.t
      ; location_state : Location_state.t
      ; mutable callees : Location.t list
      }

    let dummy =
      { call_site = Call_site.dummy; location_state = Location_state.dummy; callees = [] }
    ;;

    let rec insert list a =
      match list with
      | [] -> [ a ]
      | b :: rest ->
        (match Location.compare a b with
         | -1 -> a :: list
         | 0 ->
           (* It's tempting to use [raise_notrace] to exit early here, but in the
              _overwhelming_ majority of cases, the list has at most one element, so
              there's nothing to be gained (in particular, we don't avoid any allocation) *)
           list
         | 1 -> b :: insert rest a
         | _ -> assert false)
    ;;

    let add_callee t callee =
      let new_callees =
        (* Be sure to inline [insert] so it specializes [compare] to a cheap [Int.compare]

           This actually seems to be significantly better than just writing [insert] as a
           recursive function with [compare] hard-coded to [Location.compare]. I'm not
           sure why.
        *)
        insert t.callees callee
      in
      if not (phys_equal new_callees t.callees) then t.callees <- new_callees
    ;;
  end

  module Location_code_state = struct
    type t =
      { call_site_states : Call_site_state.t array
      ; mutable seen : bool
      }

    let dummy = { call_site_states = [||]; seen = false }
  end

  type filtered_trace = t

  type t =
    { filtered_trace : filtered_trace
    ; callback : Time_ns.Span.t -> Event.t -> unit
    ; mode : Mode.t
    ; record_call_sites : bool
    ; deferring : Raw_trace.Event.t Obj_id.Table.t
    ; collected_early : Obj_id.Hash_set.t
    ; call_sites : Call_site_state.t Call_site.Table.t
    ; locations : Location_state.t Location.Table.t
    ; location_codes : Location_code_state.t Location.Code.Table.t
    ; (* For each index [i] in the input array, the index into [current_location_codes] of
         the state of the last unskipped location code as of input [i]. (If the input
         location code at [i] wasn't skipped, this will be the state of that location
         code.) *)
      mutable input_to_location_code : int array
    ; mutable current_location_codes : Location_code_state.t array
    ; (* For each index [i] into [current_location_codes], the index into
         [current_call_sites] (and into [backtrace_buffer]) of the state of the last
         unskipped call site as of the location code at [i]. *)
      mutable location_code_to_last_call_site : int array
    ; mutable current_call_sites : Call_site_state.t array
    ; mutable backtrace_buffer : Location.t array
    ; mutable prev_in_length : int
    ; mutable prev_out_length : int
    ; mutable max_next_common_prefix : int
    ; mutable prev_domain_id : int
    }

  let create ~filtered_trace ~callback ~mode ~record_call_sites () =
    { filtered_trace
    ; callback
    ; mode
    ; record_call_sites
    ; deferring = Obj_id.Table.create ()
    ; collected_early = Obj_id.Hash_set.create ()
    ; call_sites = Call_site.Table.create ()
    ; locations = Location.Table.create ()
    ; location_codes = Location.Code.Table.create ()
    ; input_to_location_code = Array.create ~len:10 ~-1
    ; current_location_codes = Array.create ~len:10 Location_code_state.dummy
    ; location_code_to_last_call_site = Array.create ~len:10 ~-1
    ; current_call_sites = Array.create ~len:10 Call_site_state.dummy
    ; backtrace_buffer = Array.create ~len:10 Location.dummy
    ; prev_in_length = 0
    ; prev_out_length = 0
    ; max_next_common_prefix = Int.max_value
    ; prev_domain_id = -1
    }
  ;;

  let loc_cache t = t.filtered_trace.loc_cache

  let find_location_state t loc =
    Hashtbl.find_or_add t.locations loc ~default:(fun () ->
      { location = loc; seen = false })
  ;;

  let find_call_site_state t call_site =
    Hashtbl.find_or_add t.call_sites call_site ~default:(fun () : Call_site_state.t ->
      let loc =
        Filtered_location_cache.get_function_of_call_site (loc_cache t) call_site
      in
      let location_state = find_location_state t loc in
      let callees = [] in
      { call_site; location_state; callees })
  ;;

  let find_location_code_state t loc_code =
    Hashtbl.find_or_add t.location_codes loc_code ~default:(fun () ->
      let call_sites =
        Filtered_location_cache.call_sites_from_code (loc_cache t) loc_code
      in
      let call_site_states = Array.of_list_map ~f:(find_call_site_state t) call_sites in
      let seen = false in
      { call_site_states; seen })
  ;;

  let call_sites t =
    t.call_sites
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (call_site, state) ->
      let loc = state.location_state.location in
      let callees = state.callees in
      loc, call_site, callees)
    |> Call_sites.of_list
  ;;

  let enlarge array ~index ~default =
    let len = index + 1 in
    let old_len = Array.length array in
    let new_len = max len (2 * old_len) in
    let new_array = Array.create ~len:new_len default in
    Array.blito ~src:array ~dst:new_array ();
    new_array
  ;;

  let write_to_backtrace t ~index loc =
    (* We could use the [growable_array] library to guard this, but that doesn't let us
       get back the underlying array when producing an event. *)
    if index >= Array.length t.backtrace_buffer
    then t.backtrace_buffer <- enlarge t.backtrace_buffer ~index ~default:Location.dummy;
    t.backtrace_buffer.(index) <- loc
  ;;

  let set_input_to_location_code t ~index state =
    if index >= Array.length t.input_to_location_code
    then t.input_to_location_code <- enlarge t.input_to_location_code ~index ~default:~-1;
    t.input_to_location_code.(index) <- state
  ;;

  let set_current_location_code t ~index state =
    if index >= Array.length t.current_location_codes
    then
      t.current_location_codes
      <- enlarge t.current_location_codes ~index ~default:Location_code_state.dummy;
    t.current_location_codes.(index) <- state
  ;;

  let set_location_code_to_last_call_site t ~index state =
    if index >= Array.length t.location_code_to_last_call_site
    then
      t.location_code_to_last_call_site
      <- enlarge t.location_code_to_last_call_site ~index ~default:~-1;
    t.location_code_to_last_call_site.(index) <- state
  ;;

  let set_current_call_site t ~index state =
    if index >= Array.length t.current_call_sites
    then
      t.current_call_sites
      <- enlarge t.current_call_sites ~index ~default:Call_site_state.dummy;
    t.current_call_sites.(index) <- state
  ;;

  let input_to_location_code t idx =
    if idx < 0 then -1 else t.input_to_location_code.(idx)
  ;;

  let location_code_to_last_call_site t idx =
    if idx < 0 then -1 else t.location_code_to_last_call_site.(idx)
  ;;

  let record_call_site t ~call_site_state ~callee =
    if t.record_call_sites then Call_site_state.add_callee call_site_state callee
  ;;

  let conv_event (t : t) (event : Raw_trace.Event.t) : Event.t =
    match event with
    | Alloc
        { obj_id
        ; source
        ; single_allocation_size
        ; nsamples
        ; size
        ; backtrace_buffer = in_backtrace_buffer
        ; backtrace_length = in_backtrace_length
        ; common_prefix = in_common_prefix
        ; domain
        } ->
      let domain_id = (domain :> int) in
      let in_common_prefix =
        (* Discount common prefix if the domain has changed *)
        if domain_id = t.prev_domain_id then in_common_prefix else 0
      in
      let backtrace_known_to_be_truncated = in_common_prefix > in_backtrace_length in
      let in_common_prefix =
        (* The backtrace may in fact be truncated to the first [in_backtrace_length]
           frames. [in_common_prefix] will nonetheless be the number of frames in common
           between the two _true_ backtraces. We don't treat the truncated backtrace any
           differently, so for our purposes the common prefix is just the entire backtrace
           in this case. *)
        min in_common_prefix in_backtrace_length
      in
      let backtrace_length, common_prefix =
        match t.mode with
        | Preserve_times -> 0, 0
        | Preserve_backtraces ->
          let last_prev_location_code = input_to_location_code t (t.prev_in_length - 1) in
          let last_prev_call_site =
            location_code_to_last_call_site t last_prev_location_code
          in
          let in_common_prefix = min in_common_prefix t.max_next_common_prefix in
          let last_common_location_code =
            input_to_location_code t (in_common_prefix - 1)
          in
          let last_common_call_site =
            location_code_to_last_call_site t last_common_location_code
          in
          for i = last_prev_location_code downto last_common_location_code + 1 do
            let state = t.current_location_codes.(i) in
            state.seen <- false
          done;
          for i = last_prev_call_site downto last_common_call_site + 1 do
            let state = t.current_call_sites.(i) in
            state.location_state.seen <- false
          done;
          let next_location_code = ref (last_common_location_code + 1) in
          let next_call_site = ref (last_common_call_site + 1) in
          for i = in_common_prefix to in_backtrace_length - 1 do
            let location_code = in_backtrace_buffer.(i) in
            let location_code_state = find_location_code_state t location_code in
            if not location_code_state.seen
            then (
              set_current_location_code t ~index:!next_location_code location_code_state;
              location_code_state.seen <- true;
              let call_sites = location_code_state.call_site_states in
              for j = 0 to Array.length call_sites - 1 do
                let call_site_state = call_sites.(j) in
                let location_state = call_site_state.location_state in
                if not location_state.seen
                then (
                  set_current_call_site t ~index:!next_call_site call_site_state;
                  location_state.seen <- true;
                  write_to_backtrace t ~index:!next_call_site location_state.location;
                  if !next_call_site > 0
                  then (
                    let prev_call_site = t.current_call_sites.(!next_call_site - 1) in
                    record_call_site
                      t
                      ~call_site_state:prev_call_site
                      ~callee:location_state.location);
                  incr next_call_site)
              done;
              set_location_code_to_last_call_site
                t
                ~index:!next_location_code
                (!next_call_site - 1);
              incr next_location_code);
            set_input_to_location_code t ~index:i (!next_location_code - 1)
          done;
          (* Assume the last location code is for the allocation since those codes should
             only appear once in the backtrace. Exceptions: If the backtrace is obviously
             truncated (since the common prefix is too big), assume it doesn't include an
             allocation site; and if the last call site of the last location code was
             hidden by the filter, treat the allocation site as having been filtered out.
          *)
          if not (in_backtrace_length = 0 || backtrace_known_to_be_truncated)
          then (
            let last_location_code = in_backtrace_buffer.(in_backtrace_length - 1) in
            if not
                 (Filtered_location_cache.last_call_site_is_hidden
                    (loc_cache t)
                    last_location_code)
            then (
              let last_location_code_state =
                find_location_code_state t last_location_code
              in
              let call_sites = last_location_code_state.call_site_states in
              if not (Array.is_empty call_sites)
              then (
                let allocation_call_site = Array.last_exn call_sites in
                let allocation_site =
                  Filtered_location_cache.get_allocation_site_of_call_site
                    (loc_cache t)
                    allocation_call_site.call_site
                in
                write_to_backtrace t ~index:!next_call_site allocation_site;
                if !next_call_site > 0
                then (
                  let prev_call_site = t.current_call_sites.(!next_call_site - 1) in
                  record_call_site
                    t
                    ~call_site_state:prev_call_site
                    ~callee:allocation_site);
                incr next_call_site)));
          !next_call_site, last_common_call_site + 1
      in
      let backtrace_buffer = t.backtrace_buffer in
      Alloc
        { obj_id
        ; source
        ; single_allocation_size
        ; nsamples
        ; size
        ; backtrace_buffer
        ; backtrace_length
        ; common_prefix
        ; domain
        }
    | (Promote _ | Collect _ | End) as ev -> ev
  ;;

  let interpret_event t time (event : Raw_trace.Event.t) =
    match event with
    | End -> t.callback time End
    | Alloc { obj_id; _ } | Promote obj_id | Collect obj_id ->
      let { interesting; defer_minor_allocations; collect_on_promotion; _ } =
        t.filtered_trace
      in
      let return (out_event : Event.t) =
        let () =
          match t.mode with
          | Preserve_times -> ()
          | Preserve_backtraces ->
            (match event, out_event with
             | ( Alloc { backtrace_length = in_length; domain; _ }
               , Alloc { backtrace_length = out_length; _ } ) ->
               t.prev_in_length <- in_length;
               t.prev_out_length <- out_length;
               t.max_next_common_prefix <- Int.max_value;
               t.prev_domain_id <- (domain :> int)
             | Alloc _, _ | _, Alloc _ -> assert false
             | (Promote _ | Collect _ | End), (Promote _ | Collect _ | End) -> ())
        in
        t.callback time out_event
      in
      let skip () =
        (* If we don't pass the event through, we need to make sure the next common prefix
           is no larger than this one so that the next event will know to go back far
           enough to copy the backtrace from this event.
        *)
        let () =
          match t.mode with
          | Preserve_times -> ()
          | Preserve_backtraces ->
            (match event with
             | Alloc { common_prefix; domain; _ } ->
               if (domain :> int) = t.prev_domain_id
               then t.max_next_common_prefix <- min common_prefix t.max_next_common_prefix
             (* *Don't* update prev_in_length, since its purpose is to know how to move
                the out cursor *from the last event we interpreted* *)
             | Promote _ | Collect _ | End -> ())
        in
        ()
      in
      let interesting = interesting obj_id in
      if not interesting
      then skip ()
      else (
        match t.mode with
        | Preserve_backtraces -> return (event |> conv_event t)
        | Preserve_times ->
          (match event with
           | Alloc ({ obj_id; source = Minor; _ } as alloc) when defer_minor_allocations
             ->
             Hashtbl.add_exn
               t.deferring
               ~key:obj_id
               ~data:(Alloc { alloc with source = Major });
             skip ()
           | Promote obj_id when collect_on_promotion ->
             Hash_set.strict_add_exn t.collected_early obj_id;
             return (Collect obj_id)
           | Promote obj_id when defer_minor_allocations ->
             (match Hashtbl.find_and_remove t.deferring obj_id with
              | None ->
                raise
                  (Not_found_s
                     [%message "Missing deferred object" ~obj_id:((obj_id :> int) : int)])
              | Some event -> return (event |> conv_event t))
           | Collect obj_id
             when collect_on_promotion && Hash_set.mem t.collected_early obj_id ->
             Hash_set.remove t.collected_early obj_id
           | Collect obj_id when defer_minor_allocations && Hashtbl.mem t.deferring obj_id
             -> Hashtbl.remove t.deferring obj_id
           | _ -> return (event |> conv_event t)))
  ;;
end

let iter_and_return_interpreter t ~mode ~record_call_sites f =
  let interpreter =
    Interpreter.create ~filtered_trace:t ~callback:f ~mode ~record_call_sites ()
  in
  let parse_backtraces =
    match mode with
    | Mode.Preserve_backtraces -> true
    | Preserve_times -> false
  in
  Raw_trace.iter t.trace ~parse_backtraces (fun time event ->
    Interpreter.interpret_event interpreter time event);
  interpreter
;;

let iter t ~mode f =
  let interpreter = iter_and_return_interpreter t ~mode ~record_call_sites:false f in
  ignore (interpreter : Interpreter.t)
;;

let iter_and_gather_call_sites t ~mode f =
  let interpreter = iter_and_return_interpreter t ~mode ~record_call_sites:true f in
  Interpreter.call_sites interpreter
;;
