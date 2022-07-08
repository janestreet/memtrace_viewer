open! Core
open Memtrace_viewer_common
include Identifier.Make ()

let dummy = first_special
let is_dummy t = equal t dummy
