(* [main] is the main function run as the REPL loop. *)
val main : unit

(* [determine_winner game_state] goes through the [game_state] and determines who
 * won the game. *)
val determine_winner : game_state -> int
