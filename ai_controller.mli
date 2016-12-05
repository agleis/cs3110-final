open Types

(* [guess_turn p_state pool data] returns the card that this AI wishes to play
 * for the current turn, where [pool] holds the cards already played, [data]
 * holds data about the round, and [p_state] holds information about this AI.
 * Precondition: [p_state.hand] is non-empty.
 * Postcondition: Returns a card found in the list [p_state.hand]. *)
val guess_turn : player_state -> card list -> stored_data -> card

(* [pass_cards p_state] chooses three cards from the list [p_state.hand] to
 * pass to the next player during the Passing phase of the game. The actual
 * cards chosen will be different based on [p_state.ai_level].
 * Precondition: [p_state.hand] is non-empty.
 * Postcondition: Returns a list of cards of size three, each of which can
 * be found in [p_state.hand]. *)
val pass_cards : player_state -> card list
