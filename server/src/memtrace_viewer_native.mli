open! Core

val command : Command.t

module For_testing : sig
  module Substring_heavy_hitters : module type of struct
    include Substring_heavy_hitters
  end
end
