open Types
open Human_controller
open Ai_controller

let determine_winner state = 0

let rec print_cards cards =
	match cards with
		| {suit=s; value=v}::t -> (match s with
					| Heart -> let _ = print_string ((string_of_int v) ^ " of Hearts\n") in print_cards t
					| Club -> let _ = print_string ((string_of_int v) ^ " of Clubs\n") in print_cards t
					| Spade -> let _ = print_string ((string_of_int v) ^ " of Spades\n") in print_cards t
					| Diamond -> let _ = print_string ((string_of_int v) ^ " of Diamonds\n") in print_cards t)
		| [] -> ()

let rec init_deck suit v =
	if suit = 4 then []
	else if v = 15 then init_deck (suit+1) 2
	else match suit with
		| 0 -> {suit=Heart;value=v}::(init_deck suit (v+1))
		| 1 -> {suit=Club;value=v}::(init_deck suit (v+1))
		| 2 -> {suit=Spade;value=v}::(init_deck suit (v+1))
		| _ -> {suit=Diamond;value=v}::(init_deck suit (v+1))

let rec partition_list deck start length =
	match deck with
		| h::t -> if start<>0 then partition_list t (start-1) length
					else if length<> 0 then h::(partition_list t 0 (length-1))
					else []
		| [] -> []

let rec build_player_states ai_list pnum deck =
	match ai_list with
		| h::t -> let p_cards = partition_list deck (pnum*13) 13 in
				{hand=p_cards; round_points=0; game_points=0;
				 ai_level=h; collected_cards=[]}::(build_player_states t (pnum+1) deck)
		| [] -> []


let rec initialize_state ai_list deck =
	let p_states = build_player_states ai_list 0 deck in
	{pool=[]; players=p_states; phase=Pass}

let shuffle_deck deck =
	let weighted = List.map (fun x -> ((Random.int 5000), x)) deck in
	let sorted = List.sort (fun x1 x2 -> (fst x1) - (fst x2)) weighted in
	List.map (fun x -> snd x) sorted

let rec get_choices players =
	match players with
		| h::t -> if h.ai_level=0 then (Human_controller.pass_cards h)::(get_choices t)
					else (Ai_controller.pass_cards h)::(get_choices t)
		| [] -> []

let do_round state = state

let remove_cards p_list t_cards =
	List.map2 (fun p c -> {p with hand=
		(List.filter (fun x -> (List.exists (fun y->x=y) c)) p.hand) }) p_list t_cards

let reorder_cards cardlist =
	List.rev cardlist

let add_cards players cards =
	List.map2 (fun p c -> {p with hand=(p.hand@c)}) players cards

let do_trading state =
	let players = state.players in
	let traded_cards = get_choices players in
	let new_players = remove_cards players traded_cards in
	let to_add_cards = reorder_cards traded_cards in
	let fin_players = add_cards new_players to_add_cards in
	{state with players=fin_players}

let rec repl state =
	if state.phase=Pass then repl (do_trading state) else
	repl (do_round state)

let main =
	let deck = init_deck 0 2 in
	let shuffled = shuffle_deck deck in
	let init_state = initialize_state [0; 0; 0; 0] shuffled in
	let _ = print_cards shuffled in
	repl init_state

let () = main
