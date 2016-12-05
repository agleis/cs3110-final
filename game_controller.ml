open Types
open Human_controller
open Ai_controller
open Display_controller
open Game_helpers
open Unix

(* [initialize_state a n d] is a record of type game_state that is
 * takes in a list of ai_levels, a list of names, and a deck of cards
 * and initializes to the correct fields and deals the deck to the players*)
let rec initialize_state ai_list name_list deck =
	let ai_name_list = List.map2 (fun x1 x2 -> (x1,x2)) ai_list name_list in
	let p_states = build_player_states ai_name_list 0 deck in
	let next_human = find_first_human p_states in
	{pool=[]; prs=p_states; phase=Pass; round_num=0; last_human_player=next_human}

(* [get_human_cards_to_pass st p]  is a list of valid cards that player p
 * can play given game _state st. It relies on polling display_controller for input
 * and recursively calling itself until a result from display_controller is valid*)
let rec get_human_cards_to_pass st p =
	let cards = Display_controller.trade_cards [] st p in
	if are_valid_trades cards then cards
	else let () = draw_board st p in get_human_cards_to_pass st p

(* [switch_player_screen player st] is a helper function to switch_player *)
(* let switch_player_screen player st =
	let ordered_players = reorder_players_winner st.prs [] player.p_num in
	match ordered_players with
		| h::t -> let p = List.find (fun x->x.ai_level=0) t in switch_player p.name
		| _ -> switch_player player.name *)

(* [process_players_trades st ps] is a list of tuples of cards selected to
 * trade and modified player_states. It also has the side effect of calling
 * GUI draw functions and delaying so that the game can be absorbed
 * *)
let rec process_players_trades st ps =
	match ps with
		| h::t -> begin
			let is_ai = h.ai_level<>0 in
			if is_ai then begin
				(* draw board*)
				(* let () = draw_board st h in *)
				let to_pass = Ai_controller.pass_cards h in
				let new_hand = remove_cards h.hand to_pass in
				let new_p_state = {h with hand=new_hand} in
				let new_players = List.map (fun x -> if x.p_num=h.p_num then new_p_state
												else x) st.prs in
				let () = draw_board {st with prs=new_players} h in
				(* delay*)
				let () = Unix.sleep 1 in
				to_pass::(process_players_trades {st with prs=new_players} t)
			end
			else begin
			(* display board with st having last_human_player updated if need be*)
			let multi_player = (num_humans_playing st.prs) > 1 in
			let () = if multi_player then switch_player h.name else () in
			let () = draw_board {st with last_human_player=h.p_num} h in
			let to_pass = get_human_cards_to_pass {st with last_human_player=h.p_num} h in
			let new_hand = remove_cards h.hand to_pass in
			let new_p_state = {h with hand=new_hand} in
			let new_players = List.map (fun x -> if x.p_num=h.p_num then new_p_state
												else x) st.prs in
			let () = draw_board {st with last_human_player=h.p_num; prs=new_players} h in
      (* let () = Unix.sleep 1 in *)
			(* if multiple players then switch_player *)
			(* let multi_player = (num_humans_playing st.prs) > 1 in
			let () = if multi_player then switch_player_screen h st else () in *)
			to_pass::(process_players_trades {st with last_human_player=h.p_num; prs=new_players} t)
			end
		end
		| [] -> (* draw board - delay*)
				(* let () = draw_board st (List.nth st.prs ((List.length st.prs)-1)) in *)
				(* let () = Unix.sleep 1 in *)
				[]

(* [do_trading st] is the modified game_state after all 4 players have
 * traded three cards. Relies on process_players_trades. Has side effect of
 * updating the ai_data variable that is used by the ai_controller*)
let do_trading st =
	let p = st.round_num in
	let traded_cards = process_players_trades st st.prs in
	let new_players = remove_cards_players st.prs traded_cards in
	let to_add_cards = reorder_cards (p mod 3) traded_cards in
	let fin_players = add_cards new_players to_add_cards in
	{st with prs=fin_players; phase=Play}

(* [get_human_card_to_play st p data] is the valid card that player p submits
* to play. It relies on polling display_controller for input
* and recursively calling itself until a result from display_controller is valid*)
let rec get_human_card_to_play st p data =
	let pool = st.pool in
	let pnum = p.p_num in
	let has_2clubs = List.exists (fun x-> x={suit=Club; value=2}) p.hand in
	let card_to_play = Display_controller.click_card [] st p in
	if is_valid_play (List.rev pool) pnum data has_2clubs card_to_play
	then card_to_play
	else let () = draw_board st p in get_human_card_to_play st p data

(* [process_players st data ps] is the modified game_state after all players
* have chosen a card to place from thier hand into the pool. It also has the
* side effect of calling GUI draw functions and delaying so that the game
* can be absorbed *)
let rec process_players st data ps =
	match ps with
		| h::t-> begin
			let is_ai = (h.ai_level<>0) in
			if is_ai then begin
				(* draw board *)
				(* let () = draw_board st h in *)
				let pool_cards = fst (List.split st.pool) in
				let card_to_play = Ai_controller.guess_turn h pool_cards data in
				let has_2clubs = List.exists (fun x-> x={suit=Club; value=2}) h.hand in
				let new_hand = remove_cards h.hand [card_to_play] in
				let new_p_state = {h with hand=new_hand} in
				let new_players = List.map (fun x -> if x.p_num=h.p_num then new_p_state
													else x) st.prs in
				let () = draw_board {st with prs=new_players; pool=((card_to_play,h.p_num)::st.pool)} h in
				let () = Unix.sleep 1 in
				let _ = fix_ai_data card_to_play (get_ordered_p_states st.prs) data in
				(* delay *)
				let ns = {st with prs=new_players; pool=((card_to_play,h.p_num)::st.pool)} in
				process_players ns data t
			end
			else begin
				let multi_player = (num_humans_playing st.prs) > 1 in
        let () = if multi_player then switch_player h.name else () in
				let () = draw_board {st with last_human_player=h.p_num} h in
				let card_to_play = get_human_card_to_play {st with last_human_player=h.p_num} h data in
				let new_hand = remove_cards h.hand [card_to_play] in
				let new_p_state = {h with hand=new_hand} in
				let new_players = List.map (fun x -> if x.p_num=h.p_num then new_p_state
													else x) st.prs in
				let _ = fix_ai_data card_to_play (get_ordered_p_states st.prs) data in
				let ns = {st with prs=new_players;
								pool=((card_to_play,h.p_num)::st.pool);
								last_human_player=h.p_num} in
				let () = draw_board ns h in
				let () = Unix.sleep 1 in
				(* let multi_player = (num_humans_playing st.prs) > 1 in
				let () = if multi_player then switch_player_screen h st else () in *)
				process_players ns data t
			end
		end
		| [] -> st

(* [do_round st data] is the modified game_state after all players are run
* and then the round_num incremented*)
let do_round st data =
	let new_state = process_players st data st.prs in
	{new_state with phase=Play}

(* [resolve_round st data] is the modified game_state after the game decides
* who won the trick and how many points they should receive. It has the side
* effect of modifiying ai_data and calling display functions in GUI to show
* who won the trick *)
let resolve_round st data =
	let losr = get_loser ((fst (List.hd (List.rev st.pool))).suit) st.pool in
	let loser = snd losr in
	let pool_cards = fst (List.split st.pool) in
	let points = List.fold_left point_allocation 0 pool_cards in
	let new_players = List.map (fun x-> if x.p_num<>loser then x
				else {x with round_pts=x.round_pts+points}) st.prs in
	let to_change = List.nth data.players loser in
	let () = to_change.round_points<-(to_change.round_points + points) in
	let () = to_change.tricks<-(to_change.tricks@pool_cards) in
	let reorder_players = reorder_players_winner new_players [] loser in
	let () = winner st loser in
	{st with pool=[]; prs=reorder_players}

(* [repl st] is the modified st after either the PASS phase or the PLAY phase
* is run*)
let rec repl st (data:stored_data) =
	if round_over st then reflush_round st data else
	let n_st = if (st.round_num mod 4 <> 3) then st else begin
		let reord_plrs = reorder_players_2clubs st.prs [] in
		{st with prs=reord_plrs; phase=Play}
	end in
	begin
		if n_st.phase=Pass then
								let n_state = do_trading n_st in
								let _ = fix_ai_data_suits (get_ordered_p_states n_state.prs) data.players in
								let reorder_players = reorder_players_2clubs n_state.prs [] in
								let next_human = find_first_human reorder_players in
								repl {n_state with prs=reorder_players; last_human_player=next_human} data
							else
								let round_result = do_round n_st data in
								let n_state = resolve_round round_result data in
								repl n_state data
	end
and reflush_round (st:game_state) data =
	let point_list = List.map (fun x-> x.round_pts) (get_ordered_p_states st.prs) in
	let checked_for_moon = make_moon_points point_list in
	let new_players = dole_out_points st.prs checked_for_moon in
	let () = game_points new_players in
	let total_points = List.map (fun x-> x.game_points) (get_ordered_p_states new_players) in
	let did_win = List.exists (fun x-> x>=100) total_points in
	let () = if did_win then draw_end_game new_players in
	let deck = init_deck 0 2 in
	let shuffled = shuffle_deck deck in
	let init_state = initialize_state [0;0;0;0] ["";"";"";""] shuffled in
	let hands = List.map (fun x -> x.hand) init_state.prs in
	let f_players = List.map2 (fun p h -> {p with hand=h; round_pts=0}) new_players hands in
	let () = reset_ai_data data in
	let () = fix_ai_data_suits (get_ordered_p_states f_players) data.players in
	if did_win then () else
	repl { st with prs=f_players;
		round_num=(st.round_num+1);
		pool=[];
		phase=Pass} data

(* [main p_lst name_lst] is the main entry point of the program - kicks off
* repl loop*)
let main p_lst name_lst =
	let deck = init_deck 0 2 in
	let shuffled = shuffle_deck deck in
	let init_state = initialize_state p_lst name_lst shuffled in
  let () = init_window 1280 750 in
	let ai_data = build_ai_data init_state.prs in
	repl init_state ai_data
