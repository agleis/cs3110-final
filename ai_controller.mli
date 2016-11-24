(* [guess_turn player_state game_state] determines which path the AI should take
 * and updates the game and player states. *)
val guess_turn : player_state -> game_state -> player_state * game_state
