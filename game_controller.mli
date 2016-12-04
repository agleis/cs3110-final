open Types
open Human_controller
open Ai_controller

(* [main] is the main function run as the REPL loop. *)
val main : int list -> string list -> unit

val initialize_state : int list -> card list -> game_state
