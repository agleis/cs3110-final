open Types
open Randoms
open Ai_helpers
open Ai_pass

let perfect_lead_card hand data =
  if data.hearts_played
  then if has_low_hearts hand
       then play_low_heart hand
       else middle_card_from_short_suit hand
  else if (is_early hand) && (no_shorts data)
  then high_card_from_suit_not hand Heart (-1) (-1)
  else let shorted_suit = get_shorted_suit data in
  if get_short_suit hand = shorted_suit
  then (match shorted_suit with
  | HeartS _ -> high_card_from_suit_not hand Heart (-1) (-1)
  | ClubS _ -> high_card_from_suit_not hand Club (-1) (-1)
  | SpadeS _ -> high_card_from_suit_not hand Spade (-1) (-1)
  | DiamondS _ -> high_card_from_suit_not hand Diamond (-1) (-1))
  else high_card_from_suit_not hand Heart (-1) (-1)

let play_lead_card hand ai_level data =
  match ai_level with
  | 3 -> perfect_lead_card hand data
  | 2 -> if data.hearts_played
         then high_card_from_short_suit hand (-1) (-1)
         else high_card_from_suit_not hand Heart (-1) (-1)
  | _ -> random_card hand

let guess_turn_first p_state data =
  let new_hand = (if data.q_spades_played
  then p_state.hand
  else let without_k_spades =
    hand_without_card p_state.hand {suit = Spade; value = 13} in
    hand_without_card without_k_spades {suit = Spade; value = 14}) in
  if (Types.get_index (Club, 2) new_hand) >= 0
  then {suit = Club; value = 2}
  else play_lead_card new_hand p_state.ai_level data

let take_trick_if_possible hand pool =
  let high_card = highest_card_so_far pool in
  let possible_play = beat_card high_card hand in
  if possible_play.value = (-1)
  then high_card_from_suit_not hand Heart (-1) (-1)
  else possible_play

let lose_trick_if_possible hand pool =
  let high_card = highest_card_so_far pool in
  let possible_play = lose_to_card high_card hand in
  if possible_play.value = (-1)
  then high_card_from_suit_not hand Heart (-1) (-1)
  else possible_play

let guess_turn_last p_state pool data =
  match p_state.ai_level with
  | 3 ->
    if data.shooting_moon
    then take_trick_if_possible p_state.hand pool
    else lose_trick_if_possible p_state.hand pool
  | 2 -> lose_trick_if_possible p_state.hand pool
  | _ -> random_card p_state.hand

let guess_turn_second p_state pool data =
  match p_state.ai_level with
  | 3 ->
    if data.shooting_moon
    then take_trick_if_possible p_state.hand pool
    else lose_trick_if_possible p_state.hand pool
  | 2 -> lose_trick_if_possible p_state.hand pool
  | _ -> random_card p_state.hand

let guess_turn_third p_state pool data =
  match p_state.ai_level with
  | 3 ->
    if data.shooting_moon
    then take_trick_if_possible p_state.hand pool
    else lose_trick_if_possible p_state.hand pool
  | 2 -> lose_trick_if_possible p_state.hand pool
  | _ -> random_card p_state.hand

let guess_turn p_state pool data =
  match List.length pool with
  | 0 -> guess_turn_first p_state data
  | 1 -> guess_turn_second p_state pool data
  | 2 -> guess_turn_third p_state pool data
  | _ -> guess_turn_last p_state pool data

let pass_cards p_state =
  let spade_state = get_spade_state p_state.hand in
  match spade_state with
  | Ace_and_king (a, k) -> ({suit = Spade; value = 14})::
                        ({suit = Spade; value = 13})::
                        (get_last_card p_state.hand p_state.ai_level a k)::[]
  | Ace i -> ({suit = Spade; value = 14})::
             (get_two_cards p_state.hand p_state.ai_level i)
  | King i -> ({suit = Spade; value = 14})::
              (get_two_cards p_state.hand p_state.ai_level i)
  | None -> get_three_cards p_state.hand p_state.ai_level

let initialize () =
  let data = {
    has_clubs = [true; true; true];
    has_spades = [true; true; true];
    has_diamonds = [true; true; true];
    has_hearts = [true; true; true];
    q_spades_played = false;
    hearts_played = false;
    shooting_moon = true;
    tricks_p1 = [];
    tricks_p2 = [];
    tricks_p3 = [];
  } in data
