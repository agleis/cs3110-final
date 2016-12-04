open Types
open Randoms
open Ai_helpers
open Ai_pass
(* open Ai_probabilites *)

let last_card lst =
  match List.length lst with
  | 0 -> {suit = Diamond; value = (-1)}
  | n -> List.nth lst (n-1)

let card_to_play game_state player_state = {suit=Heart; value=2}

let play_non_low_heart hand data =
if (match get_short_suit hand with SpadeS _ -> true | _ -> false)
then if has_card_of_suit Diamond hand
     then high_card_from_suit hand Diamond (-1) (-1)
     else if has_card_of_suit Club hand
     then high_card_from_suit hand Club (-1) (-1)
     else low_card_from_suit hand Spade (-1) (-1)
else if (is_early hand) && (no_shorts data)
then high_card_from_suit_not hand Heart (-1) (-1)
else let shorted_suit = get_shorted_suit data in
if get_short_suit hand = shorted_suit
then (match shorted_suit with
| HeartS _ -> middle_card_from_suit_not hand Heart (-1) (-1)
| ClubS _ -> middle_card_from_suit_not hand Club (-1) (-1)
| SpadeS _ -> middle_card_from_suit_not hand Spade (-1) (-1)
| DiamondS _ -> middle_card_from_suit_not hand Diamond (-1) (-1))
else middle_card_from_suit_not hand Heart (-1) (-1)

let perfect_lead_card hand data =
  if data.hearts_played
  then if has_low_hearts hand
       then play_low_heart hand
       else play_non_low_heart hand data
  else play_non_low_heart hand data

let play_lead_card hand ai_level data =
  match ai_level with
  | 3 -> perfect_lead_card hand data
  | 2 -> if data.hearts_played
         then high_card_from_short_suit hand (-1) (-1)
         else high_card_from_suit_not hand Heart (-1) (-1)
  | _ -> random_card hand Club

let guess_turn_first p_state data =
  let new_hand = (if data.q_spades_played ||
                     get_index (Spade, 12) p_state.hand >= 0 ||
                     List.length p_state.hand <= 3
  then p_state.hand
  else let without_k_spades =
    hand_without_card p_state.hand {suit = Spade; value = 13} in
    let intermed_hand =
      hand_without_card without_k_spades {suit = Spade; value = 14} in
    hand_without_card intermed_hand {suit = Spade; value = 12}) in
  if (Types.get_index (Club, 2) new_hand) >= 0
  then {suit = Club; value = 2}
  else play_lead_card new_hand p_state.ai_level data

let lose_trick_if_possible hand pool =
  let lead_suit = (last_card pool).suit in
  let high_card = highest_card_so_far pool lead_suit in
  let possible_play = lose_to_card high_card hand in
  if possible_play.value = (-1)
  then high_card_from_suit_not hand Heart (-1) (-1)
  else possible_play

let take_trick_if_possible hand pool =
  let lead_suit = (last_card pool).suit in
  let has_points = contains_heart_or_q_spades pool in
  if not has_points then lose_trick_if_possible hand pool else
  let high_card = highest_card_so_far pool lead_suit in
  let possible_play = beat_card high_card hand in
  if possible_play.value = (-1)
  then screw_other_player hand lead_suit
  else possible_play

let take_trick_max hand pool data =
  let lead_suit = (last_card pool).suit in
  let high_card = highest_card_so_far pool lead_suit in
  let prob_list = beating_cards_still_out high_card pool data in
  let beating_cards = high_card::prob_list in
  lowest_beating_card beating_cards hand data.hearts_played

let lose_trick_max hand pool data =
  let lead_suit = (last_card pool).suit in
  let high_card = highest_card_so_far pool lead_suit in
  let played_card = highest_losing_card high_card hand data.hearts_played in
  if played_card.value = (-1)
  then low_card_from_suit hand lead_suit (-1) (-1)
  else played_card

let guess_turn_last p_state pool data =
  let lead_suit = (last_card pool).suit in
  match p_state.ai_level with
  | 3 ->
    if (List.nth data.players p_state.p_num).shooting_moon
    then take_trick_if_possible p_state.hand pool
    else lose_trick_if_possible p_state.hand pool
  | 2 -> lose_trick_if_possible p_state.hand pool
  | _ -> random_card p_state.hand lead_suit

let guess_turn_second p_state pool data =
  let lead_suit = (last_card pool).suit in
  let new_hand = (if data.q_spades_played ||
                     get_index (Spade, 12) p_state.hand >= 0 ||
                     List.length p_state.hand <= 2 ||
                     (lead_suit = Spade &&
                      cards_of_suit lead_suit p_state.hand <= 2)
  then p_state.hand
  else let without_k_spades =
    hand_without_card p_state.hand {suit = Spade; value = 13} in
    hand_without_card without_k_spades {suit = Spade; value = 14}) in
  match p_state.ai_level with
  | 3 ->
    if (List.nth data.players p_state.p_num).shooting_moon
    then take_trick_max new_hand pool data
    else lose_trick_max new_hand pool data
  | 2 -> lose_trick_if_possible new_hand pool
  | _ -> random_card p_state.hand lead_suit

let guess_turn_third p_state pool data =
  let lead_suit = (last_card pool).suit in
  let new_hand = (if data.q_spades_played ||
                     get_index (Spade, 12) p_state.hand >= 0 ||
                     List.length p_state.hand <= 2 ||
                     (lead_suit = Spade &&
                      cards_of_suit lead_suit p_state.hand <= 2)
  then p_state.hand
  else let without_k_spades =
    hand_without_card p_state.hand {suit = Spade; value = 13} in
    hand_without_card without_k_spades {suit = Spade; value = 14}) in
  match p_state.ai_level with
  | 3 ->
    if (List.nth data.players p_state.p_num).shooting_moon
    then take_trick_max new_hand pool data
    else lose_trick_max new_hand pool data
  | 2 -> lose_trick_if_possible new_hand pool
  | _ -> random_card p_state.hand lead_suit

let guess_turn p_state pool data =
  let _ = check_if_shooting_moon p_state.hand pool data p_state.p_num in
  let possible_card =
    match List.length pool with
    | 0 -> guess_turn_first p_state data
    | 1 -> guess_turn_second p_state pool data
    | 2 -> guess_turn_third p_state pool data
    | _ -> guess_turn_last p_state pool data in
  if possible_card.value = (-1)
  then if List.length pool = 0
       then random_card_no_suit p_state.hand
       else let lead_suit = (last_card pool).suit in
       random_card p_state.hand lead_suit
  else possible_card

let pass_cards p_state =
  let spade_state = get_spade_state p_state.hand in
  match spade_state with
  | Ace_and_king (a, k) ->
    let one_hand = hand_without_card p_state.hand {suit = Spade; value = 14} in
    let new_hand = hand_without_card one_hand {suit = Spade; value = 13} in
    ({suit = Spade; value = 14})::
    ({suit = Spade; value = 13})::
    (get_last_card new_hand p_state.ai_level a k)::[]
  | Ace i ->
    let new_hand = hand_without_card p_state.hand {suit = Spade; value = 14} in
    ({suit = Spade; value = 14})::(get_two_cards new_hand p_state.ai_level i)
  | King i ->
    let new_hand = hand_without_card p_state.hand {suit = Spade; value = 13} in
    ({suit = Spade; value = 13})::(get_two_cards new_hand p_state.ai_level i)
  | None -> get_three_cards p_state.hand p_state.ai_level

let initialize () =
  let data = {
    q_spades_played = false;
    hearts_played = false;
    players = [{
      has_clubs = true;
      has_spades = true;
      has_diamonds = true;
      has_hearts = true;
      shooting_moon = false;
      tricks = [];
      round_points = 0;
    }; {
      has_clubs = true;
      has_spades = true;
      has_diamonds = true;
      has_hearts = true;
      shooting_moon = false;
      tricks = [];
      round_points = 0;
    }; {
      has_clubs = true;
      has_spades = true;
      has_diamonds = true;
      has_hearts = true;
      shooting_moon = false;
      tricks = [];
      round_points = 0;
    }; {
      has_clubs = true;
      has_spades = true;
      has_diamonds = true;
      has_hearts = true;
      shooting_moon = false;
      tricks = [];
      round_points = 0;
    }]
  } in data
