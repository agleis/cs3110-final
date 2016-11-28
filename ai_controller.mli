open Types

(* [guess_turn p_state pool data] determines which path the AI should take
 * and updates the game and player states. *)
val guess_turn : player_state -> card list -> stored_data -> card

val pass_cards : player_state -> card list
