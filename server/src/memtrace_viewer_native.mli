open! Core

val command : Command.t

module User_state = User_state

module For_testing : sig
  module Substring_heavy_hitters : module type of struct
    include Substring_heavy_hitters
  end

  module Filtered_trace = Filtered_trace
  module Location = Location
  module Raw_trace = Raw_trace
end
