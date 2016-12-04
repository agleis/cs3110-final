open Types
open Randoms
open Ai_helpers

(* [spade_state] represents the types of high-ranked Spades that a hand has,
 * where the parametrized values represent the indices of their respective
 * cards. *)
type spade_state = Ace of int | King of int | Ace_and_king  of int * int| None

(* [short_suit] represents a suit, and the parametrized value represents the
 * number of that suit ina  given hand. *)
type short_suit = SpadeS of int | ClubS of int | HeartS of int | DiamondS of int

(* [suit_values] gives a list of the values of each card of the four suits
 * in a given hand. *)
type suit_values = {
  mutable spade: int list;
  mutable club: int list;
  mutable heart: int list;
  mutable diamond: int list;
}

(* [get_spade_state hand] returns a value of type spade_state, representing
 * which (if any) of the high spade cards are in list [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let get_spade_state hand =
  let king = Types.get_index (Spade, 13) hand in
  let ace = Types.get_index (Spade, 14) hand in
  if king >= 0
  then if ace >= 0
       then Ace_and_king (ace, king)
       else King king
  else if ace >= 0
       then Ace ace
       else None

(* [get_q_spades_or_short_suit hand ind1 ind2] returns the Queen of Spades
 * if it is in [hand], or a card from the short suit of [hand] that is not
 * located at either index [ind1] or index [ind2] in [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: Will not return a card at index [ind1] or [ind2] in [hand]. *)
let get_q_spades_or_short_suit hand ind1 ind2 =
  let q_spades_ind = Types.get_index (Spade, 12) hand in
  if q_spades_ind >= 0
  then {suit = Spade; value = 12}
  else (Ai_helpers.high_card_from_short_suit hand ind1 ind2)

(* [get_q_spades_or_two_short_suit hand ind] returns the Queen of Spades
 * if it is in [hand], and up to two cards from the short suit of [hand] that
 * are not located at index [ind] in [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: Will not return a card at index [ind] in [hand]. *)
let get_q_spades_or_two_short_suit hand ind =
  let q_spades_ind = Types.get_index (Spade, 12) hand in
  if q_spades_ind >= 0
  then {suit = Spade; value = 12}::(Ai_helpers.high_card_from_short_suit hand ind (-1))::[]
  else let first_card = (Ai_helpers.high_card_from_short_suit hand ind (-1)) in
  let new_hand = Ai_helpers.hand_without_card hand first_card in
  first_card::(Ai_helpers.high_card_from_short_suit new_hand ind (-1))::[]

(* [get_q_spades_or_three_short_suit hand] returns the Queen of Spades
 * if it is in [hand], and up to three cards from the short suit of [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let get_q_spades_or_three_short_suit hand =
  let q_spades_ind = Types.get_index (Spade, 12) hand in
  let first_card = (if q_spades_ind >= 0
  then {suit = Spade; value = 12}
  else (Ai_helpers.high_card_from_short_suit hand (-1) (-1))) in
  let new_hand = Ai_helpers.hand_without_card hand first_card in
  let second_card = Ai_helpers.high_card_from_short_suit new_hand (-1) (-1) in
  let third_hand = Ai_helpers.hand_without_card new_hand second_card in
  let third_card = Ai_helpers.high_card_from_short_suit third_hand (-1) (-1) in
  [first_card; second_card; third_card]

(* [get_highest_card_not suit v hand] gets the highest-ranked card of suit
 * [suit] in [hand] whose value is not equal to [v].
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let rec get_highest_card_not suit v hand =
  let suit_list = List.filter (fun c -> c.suit = suit) hand in
  let sorted_suit_list = List.sort (compare_cards false) suit_list in
  match sorted_suit_list with
  | [] -> {suit = Diamond; value = (-1)}
  | h::t -> (if h.value = v then get_highest_card_not suit v t else h)

(* [get_highest_heart_or_other hand] returns the highest Heart (except the Ace),
 * or the highest Club if there are no Hearts, or the highest Diamond if there
 * are no Clubs, or the highest Spade (except the Queen).
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let get_highest_heart_or_other hand =
  let heart = get_highest_card_not Heart 14 hand in
  if heart.value >= 2 then heart else
  let club = get_highest_card_not Club (-1) hand in
  if club.value >= 2 then club else
  let diamond = get_highest_card_not Diamond (-1) hand in
  if diamond.value >= 2 then diamond else
  let spade = get_highest_card_not Spade 12 hand in
  if spade.value >= 2 then spade else {suit = Diamond; value = (-1)}

(* [get_highest_heart_or_other hand] returns the highest Spade
 * (except the Queen), or the highest Club if there are no Hearts,
 * or the highest Diamond if there are no Clubs,
 * or the highest Heart (except the Ace).
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let get_highest_spade_or_other hand =
  let spade = get_highest_card_not Spade 12 hand in
  if spade.value >= 12 then spade else
  let club = get_highest_card_not Club (-1) hand in
  if club.value >= 2 then club else
  let diamond = get_highest_card_not Diamond (-1) hand in
  if diamond.value >= 2 then diamond else
  let heart = get_highest_card_not Heart 14 hand in
  if heart.value >= 2 then heart else {suit = Diamond; value = (-1)}

(* [perfect_card hand] returns the ideal card to trade from card list [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let perfect_card hand =
  let two_clubs_ind = Types.get_index (Club, 2) hand in
  if two_clubs_ind >= 0
  then {suit = Club; value = 2}
  else
    match get_short_suit hand with
    | HeartS _ -> get_highest_heart_or_other hand
    | ClubS i when i = 1 -> get_highest_card_not Diamond (-1) hand
    | ClubS i -> get_highest_card_not Club (-1) hand
    | DiamondS _ -> get_highest_card_not Diamond (-1) hand
    | SpadeS _ -> get_highest_spade_or_other hand

(* [two_perfect_cards hand] returns the ideal two cards
 * to trade from card list [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let two_perfect_cards hand =
  let first_card = perfect_card hand in
  let new_hand = Ai_helpers.hand_without_card hand first_card in
  let second_card = perfect_card new_hand in
  first_card::second_card::[]

(* [three_perfect_cards hand] returns the ideal three cards
 * to trade from card list [hand].
 * Precondition: [hand] is non-empty.
 * Postcondition: None. *)
let three_perfect_cards hand =
  let q_spades_ind = Types.get_index (Spade, 12) hand in
  let first_card = (if q_spades_ind >= 0
  then {suit = Spade; value = 12}
  else perfect_card hand) in
  let new_hand = Ai_helpers.hand_without_card hand first_card in
  first_card::(two_perfect_cards new_hand)

(* [get_last_card hand ai_level ind1 ind2] returns a single card to be traded
 * from [hand], the value of which is based on [ai_level].
 * Precondition: [hand] is non-empty.
 * Postcondition: Returns a card from [hand] that is not at index [ind1] or
 * [ind2] in [hand]. *)
let get_last_card hand ai_level ind1 ind2 =
  match ai_level with
  | 3 -> perfect_card hand
  | 2 -> get_q_spades_or_short_suit hand ind1 ind2
  | _ -> get_random_not_two hand ind1 ind2

(* [get_two_cards hand ai_level ind] returns a list of 2 cards to be traded
 * from [hand], the value of which are based on [ai_level].
 * Precondition: [hand] is non-empty.
 * Postcondition: Returns exactly two cards from [hand]
 * that are not at index [ind]. *)
let get_two_cards hand ai_level ind =
  match ai_level with
  | 3 -> two_perfect_cards hand
  | 2 -> get_q_spades_or_two_short_suit hand ind
  | _ -> get_two_random_not hand ind

(* [get_three_cards hand ai_level ind] returns a list of 3 cards to be traded
 * from [hand], the value of which are based on [ai_level].
 * Precondition: [hand] is non-empty.
 * Postcondition: Returns exactly three cards from [hand]. *)
let get_three_cards hand ai_level =
  match ai_level with
  | 3 -> three_perfect_cards hand
  | 2 -> get_q_spades_or_three_short_suit hand
  | _ -> get_three_random hand
