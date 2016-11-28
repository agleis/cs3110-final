open Types

let get_input player_state = ()

(* [process_line string] processes a line of input that the player enters and
 * returns the new state. *)
let process_line p_state input = p_state

let pass_cards p_state = List.find_all (fun x -> x.suit = Heart) p_state.hand

let card_to_play = {suit=Heart; value=2}
