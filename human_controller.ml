open Types

let get_input player_state = ()

(* [process_line string] processes a line of input that the player enters and
 * returns the new state. *)
let process_line p_state input = p_state

let pass_cards p_state =
	let i1 = read_int () in
	let i2 = read_int () in
	let i3 = read_int () in
	(List.nth p_state.hand i1)::
	(List.nth p_state.hand i2)::
	(List.nth p_state.hand i3)::[]

let valid_move c = true


let rec card_to_play p_state =
	let input = read_int () in
	let card = List.nth p_state.hand input in
	if valid_move card then card else card_to_play p_state
