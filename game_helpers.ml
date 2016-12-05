open Types

(* Legacy printing *)
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

(* Deck building helpers *)
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

(* code inspired from http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)
let shuffle_deck deck =
  Random.self_init ();
	let weighted = List.map (fun x -> ((Random.int 5000), x)) deck in
	let sorted = List.sort (fun x1 x2 -> (fst x1) - (fst x2)) weighted in
	List.map (fun x -> snd x) sorted
(*-------*)


(* Player state manipulation helpers *)
let rec num_humans_playing ps =
	match ps with
		| h::t -> if h.ai_level=0 then 1 + num_humans_playing t else num_humans_playing t
		| [] -> 0

let rec find_first_human ps =
	match ps with
		| h::t -> if h.ai_level=0 then h.p_num else find_first_human t
		| [] -> failwith "No humans"

let rec reorder_players_2clubs players acc =
	match players with
		| h::t -> if List.exists (fun x->x={suit=Club; value=2}) h.hand then
					(h::t)@acc else reorder_players_2clubs t acc@[h]
		| [] -> acc

let rec reorder_players_winner ps acc winner=
	match ps with
		| h::t -> if h.p_num=winner then (h::t)@(List.rev acc) else reorder_players_winner t (h::acc) winner
		| [] -> []

let get_ordered_p_states players =
	List.sort (fun x1 x2 -> x1.p_num - x2.p_num) players


let remove_cards main_list to_remove =
	List.filter (fun x -> not (List.exists (fun y -> x=y) to_remove)) main_list

let reorder_cards which to_trade =
	match to_trade with
		| [c0; c1; c2; c3] -> begin
			match which with
				| 0 -> [c1; c2; c3; c0]
				| 1 -> [c3; c0; c1; c2]
				| 2 -> [c2; c3; c0; c1;]
				| _ -> failwith "Invalid trade round type"
		end
		| _ -> failwith "Invalid trade option"

let add_cards players cards =
	List.map2 (fun p c -> {p with hand=(p.hand@c)}) players cards

let remove_cards_players p_states to_remove =
	List.map2 (fun p t -> {p with hand=(remove_cards p.hand t)})
		p_states to_remove

(* Game state logic helpers *)
let round_over st =
	match st.prs with
		| h::t -> (List.length h.hand) = 0
		| [] -> true

let suit_available su pdata =
	match su with
		| Heart -> pdata.has_hearts
		| Club -> pdata.has_clubs
		| Spade -> pdata.has_spades
		| Diamond -> pdata.has_diamonds

let is_valid_play pool pnum data has_2clubs crd =
	match pool with
		| (c, pn)::t -> c.suit = crd.suit || not (suit_available c.suit (List.nth data.players pnum))
		| [] -> (not has_2clubs || crd={suit=Club; value=2}) && (crd.suit<>Heart || data.hearts_played)

let rec valid_helper cards =
	match cards with
	| h1::h2::t -> h1<>h2 && valid_helper (h2::t)
	| h::t -> true
	| [] -> true

let rec are_valid_trades cards =
	let sorted = List.sort (fun x1 x2 -> compare_cards true x1 x2) cards in
	valid_helper sorted

let get_loser su cards =
	let sorted = List.sort (fun x1 x2-> compare_cards_with_suit su (fst x2) (fst x1)) cards in
	List.hd sorted

let point_allocation acc y =
	if y.suit=Heart then acc + 1
	else if y={suit=Spade;value=12} then acc + 13
	else acc

let rec make_moon_h points =
	match points with
		| 26::t -> 0::(make_moon_h t)
		| h::t -> 26::(make_moon_h t)
		| [] -> []

let make_moon_points points =
	if List.exists (fun x -> x=26) points then
		make_moon_h points else points

let dole_out_points (players:player_state list) points =
	List.map2 (fun pl pts -> {pl with game_points=(pl.game_points+pts); round_pts=pts})
		(get_ordered_p_states players) points
(* Game ai_data manipulation *)
let fix_ai_data_suits players (data:player_data list) =
	List.iter2 (fun p d ->
			d.has_clubs <- (List.exists (fun x -> x.suit=Club) p.hand);
			d.has_spades <- (List.exists (fun x -> x.suit=Spade) p.hand);
			d.has_diamonds <- (List.exists (fun x -> x.suit=Diamond) p.hand);
			d.has_hearts <- (List.exists (fun x -> x.suit=Heart) p.hand)) players data

let fix_ai_data (crd:card) (ps:player_state list) (dt:stored_data) =
	let () = if crd.suit=Heart then dt.hearts_played<-true
			else if crd={suit=Spade; value=12} then dt.q_spades_played<-true else () in
	fix_ai_data_suits ps dt.players

let reset_players_data prs =
	match prs with
		| h::t -> begin
			h.has_clubs <- false;
			h.has_spades <- false;
			h.has_diamonds <- false;
			h.has_hearts <- false;
			h.shooting_moon <- false;
			h.tricks <- [];
			h.round_points <- 0;
		end
		| [] -> ()

let reset_ai_data dt =
	let () = dt.hearts_played<-false in
	let () = dt.q_spades_played<-false in
	let () = reset_players_data dt.players in
	()
(* Game state init helpers *)
let rec build_player_states ai_name_list pnum deck =
	match ai_name_list with
		| (a, n)::t -> let p_cards = partition_list deck (pnum*13) 13 in
				{hand=p_cards; game_points=0; round_pts=0; name=n;
				 ai_level=a; collected_cards=[]; p_num=pnum}::(build_player_states t (pnum+1) deck)
		| [] -> []

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
