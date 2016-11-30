open Types
open Human_controller
open Ai_controller

let determine_winner st = 0

let round_over st =
	match st.prs with
		| h::t -> (List.length h.hand) = 0
		| [] -> true

let rec print_cards cards =
	match cards with
		| {suit=s; value=v}::t -> (match s with
					| Heart -> let _ = print_string ((string_of_int v) ^ " of Hearts\n") in print_cards t
					| Club -> let _ = print_string ((string_of_int v) ^ " of Clubs\n") in print_cards t
					| Spade -> let _ = print_string ((string_of_int v) ^ " of Spades\n") in print_cards t
					| Diamond -> let _ = print_string ((string_of_int v) ^ " of Diamonds\n") in print_cards t)
		| [] -> ()

let print_game st =
	let _ = print_string "pool:\n" in
	let _ = print_cards (fst (List.split st.pool)) in
	List.iter (fun x -> let _ = print_string ("\nPlayer "^ (string_of_int x.p_num)^" ----- "^(string_of_int x.game_points)^" pts\n") in print_cards x.hand) st.prs

let rec init_deck su v =
	if su = 4 then []
	else if v = 15 then init_deck (su+1) 2
	else match su with
		| 0 -> {suit=Heart;value=v}::(init_deck su (v+1))
		| 1 -> {suit=Club;value=v}::(init_deck su (v+1))
		| 2 -> {suit=Spade;value=v}::(init_deck su (v+1))
		| _ -> {suit=Diamond;value=v}::(init_deck su (v+1))

let rec partition_list deck start length =
	match deck with
		| h::t -> if start<>0 then partition_list t (start-1) length
					else if length<> 0 then h::(partition_list t 0 (length-1))
					else []
		| [] -> []

let rec build_player_states ai_list pnum deck =
	match ai_list with
		| h::t -> let p_cards = partition_list deck (pnum*13) 13 in
				{hand=p_cards; game_points=0;
				 ai_level=h; collected_cards=[]; p_num=pnum}::(build_player_states t (pnum+1) deck)
		| [] -> []

let rec initialize_state ai_list deck =
	let p_states = build_player_states ai_list 0 deck in
	{pool=[]; prs=p_states; phase=Pass; round_num=1}

let build_single_data player =
	{
		has_clubs = (List.exists (fun x -> x.suit=Club) player.hand);
		has_spades = (List.exists (fun x -> x.suit=Spade) player.hand);
		has_diamonds = (List.exists (fun x -> x.suit=Diamond) player.hand);
		has_hearts = (List.exists (fun x -> x.suit=Heart) player.hand);
		shooting_moon = false;
		round_points = 0;
		tricks = []
	}

let rec build_ai_players players =
	match players with
		| h::t -> (build_single_data h)::(build_ai_players t)
		| [] -> []

let build_ai_data players : stored_data =
	{players=(build_ai_players players);
		q_spades_played=false; hearts_played=false}

let get_ordered_p_states players =
	List.sort (fun x1 x2 -> x1.p_num - x2.p_num) players

let fix_ai_data_suits players (data:player_data list) =
	let _ = print_string ("Players length: "^(string_of_int (List.length players))^"\n") in
	let _ = print_string ("Data length: "^(string_of_int (List.length data))^"\n") in
	List.iter2 (fun p d ->
			d.has_clubs <- (List.exists (fun x -> x.suit=Club) p.hand);
			d.has_spades <- (List.exists (fun x -> x.suit=Spade) p.hand);
			d.has_diamonds <- (List.exists (fun x -> x.suit=Diamond) p.hand);
			d.has_hearts <- (List.exists (fun x -> x.suit=Heart) p.hand)) players data

let shuffle_deck deck =
	let weighted = List.map (fun x -> ((Random.int 5000), x)) deck in
	let sorted = List.sort (fun x1 x2 -> (fst x1) - (fst x2)) weighted in
	List.map (fun x -> snd x) sorted

let rec get_choices players =
	match players with
		| h::t -> begin
			let _ = print_string ("\n\nPlayer "^(string_of_int h.p_num)^"'s turn - trading\n") in
			if h.ai_level=0 then let x = Human_controller.pass_cards h in x::(get_choices t)
					else let x = Ai_controller.pass_cards h in x::(get_choices t)
		end
		| [] -> []

let remove_cards main_list to_remove =
	List.filter (fun x -> not (List.exists (fun y -> x=y) to_remove)) main_list

let reorder_cards cardlist =
	List.rev cardlist

let add_cards players cards =
	List.map2 (fun p c -> {p with hand=(p.hand@c)}) players cards

let remove_cards_players p_states to_remove =
	List.map2 (fun p t -> {p with hand=(remove_cards p.hand t)})
		p_states to_remove

let do_trading st =
	let p = st.round_num in
	let traded_cards = get_choices st.prs in
	let t = List.nth traded_cards 0 in
	let _ = print_string "\nP0 traded these cards ---\n" in
	let _ = print_cards t in
	let new_players = remove_cards_players st.prs traded_cards in
	let p0hand = (List.nth new_players 0).hand in
	let _ = print_string "\nP0 hand ---\n" in
	let _ = print_cards p0hand in
	let to_add_cards = reorder_cards traded_cards in
	let fin_players = add_cards new_players to_add_cards in
	{st with prs=fin_players; phase=Play}

let rec build_pool const_ps ps cur_pool data =
	match ps with
		| h::t -> begin
			let _ = print_string ("\n\nPlayer "^(string_of_int h.p_num)^"'s turn\n") in
			let card_to_play = if h.ai_level=0 then Human_controller.card_to_play h
						else Ai_controller.guess_turn h cur_pool data in
			let new_hand = remove_cards h.hand [card_to_play] in
			let new_p_state = {h with hand=new_hand} in
			let _ = fix_ai_data_suits (get_ordered_p_states const_ps) data.players in
			(card_to_play, new_p_state)::(build_pool const_ps t (card_to_play::cur_pool) data)
		end
		| [] -> []

let do_round st data =
	let pool_players = build_pool st.prs st.prs [] data in
	let n_pool = List.map (fun x-> (fst x, (snd x).p_num)) pool_players in
	let split = List.split pool_players in
	let n_players = snd split in
	{prs=n_players; pool=n_pool; round_num=st.round_num+1; phase=Play}

let get_loser su cards =
	let sorted = List.sort (fun x1 x2-> compare_cards_with_suit su (fst x1) (fst x2)) cards in
	List.hd sorted

let point_allocation acc y =
	if y.suit=Heart then acc + 1
	else if y={suit=Spade;value=12} then acc + 13
	else acc

let resolve_round st data =
	let loser = get_loser ((fst (List.hd st.pool)).suit) st.pool in
	let loser_player = snd loser in
	let pool_cards = fst (List.split st.pool) in
	let points = List.fold_left point_allocation 0 pool_cards in
	let to_change = List.nth data.players loser_player in
	let () = to_change.round_points<-(to_change.round_points + points) in
	let () = to_change.tricks<-(to_change.tricks@pool_cards) in
	{st with pool=[]}

let rec make_moon_h points =
	match points with
		| 26::t -> 0::(make_moon_h t)
		| h::t -> 26::(make_moon_h t)
		| [] -> []

let make_moon_points points =
	if List.exists (fun x -> x=26) points then
		make_moon_h points else points

let dole_out_points (players:player_state list) points =
	List.map2 (fun pl pts -> {pl with game_points=(pl.game_points+pts)})
		players points

let rec repl st (data:stored_data) =
	let _ = print_game st in
	if round_over st then reflush_round st data else
	begin
		if st.phase=Pass then
								let n_state = do_trading st in
								let _ = fix_ai_data_suits n_state.prs data.players in
								repl n_state data
							else
								let round_result = do_round st data in
								let n_state = resolve_round round_result data in
								repl n_state data
	end
and reflush_round (st:game_state) data =
	let trick_list = List.map (fun x->x.tricks) data.players in
	let point_list = List.map (fun x-> (List.fold_left point_allocation 0 x)) trick_list in
	let _ = List.iter (fun x-> print_string ("pts: "^(string_of_int x)^"\n")) point_list in
	let checked_for_moon = make_moon_points point_list in
	let _ = List.iter (fun x-> print_string ("points: "^(string_of_int x)^"\n")) checked_for_moon in
	let new_players = dole_out_points st.prs checked_for_moon in
	let deck = init_deck 0 2 in
	let shuffled = shuffle_deck deck in
	let init_state = initialize_state [0;0;0;0] shuffled in
	let hands = List.map (fun x -> x.hand) init_state.prs in
	let f_players = List.map2 (fun p h -> {p with hand=h}) new_players hands in
	repl {st with
		prs=f_players;
		round_num=(st.round_num+1);
		pool=[];
		phase=Pass} data

let main =
	let deck = init_deck 0 2 in
	let shuffled = shuffle_deck deck in
	let init_state = initialize_state [0; 0; 0; 0] shuffled in
	let ai_data = build_ai_data init_state.prs in
	let _ = print_cards shuffled in
	repl init_state ai_data

let () = main
