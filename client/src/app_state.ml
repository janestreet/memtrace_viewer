open! Core
open Bonsai_web_proc
open Memtrace_viewer_common

module Default_selection = struct
  type t =
    | First_caller
    | First_callee
    | No_selection
  [@@deriving sexp, equal]
end

module State = struct
  type t = { focus_backtrace : Data.Backtrace.t } [@@deriving sexp, equal]

  let default =
    let focus_backtrace = [ Data.Location.toplevel ] in
    { focus_backtrace }
  ;;

  let focus ~trie t =
    match Data.Fragment_trie.find trie t.focus_backtrace with
    | None -> Data.Fragment_trie.empty_fragment trie
    | Some focus -> focus
  ;;
end

type t =
  { focus : Data.Fragment.t
  ; set_focus :
      Data.Fragment.t
      -> default_selection:Default_selection.t
      -> reset_selection:
           (Data.Fragment.t -> default_selection:Default_selection.t -> unit Effect.t)
      -> unit Effect.t
  }

let component ~trie =
  let open Bonsai.Let_syntax in
  let%sub state, set_state = Bonsai.state State.default ~equal:[%equal: State.t] in
  let focus =
    let%map state and trie in
    State.focus ~trie state
  in
  let set_focus =
    let%map set_state in
    fun new_focus ~default_selection ~reset_selection ->
      let focus_backtrace = Data.Fragment.backtrace new_focus in
      Effect.Many
        [ set_state { focus_backtrace }; reset_selection new_focus ~default_selection ]
  in
  let%arr focus and set_focus in
  { focus; set_focus }
;;
