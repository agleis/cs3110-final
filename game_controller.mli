open Types
open Human_controller
open Ai_controller

(* [main] is the main function run as the REPL loop. *)
val main : int list -> unit

(* [determine_winner game_state] goes through the [game_state] and determines who
 * won the game. *)
val determine_winner : game_state -> int

val initialize_state : int list -> card list -> game_state
