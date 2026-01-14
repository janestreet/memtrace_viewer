open! Core

val main : unit -> unit

module App = App
module Server_state = Server_state

module For_testing : sig
  module Graph_view = Graph_view
end
