(* [get_input] waits for the player with state
 * [player_state] to input a command. *)
val get_input : player_state -> unit

(* [process_line string] processes a line of input that the player enters and
 * returns the new state. *)
val process_line : player_state -> string -> player_state
