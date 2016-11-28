(* [game_state] is the type of a game_controller. *)

type suit = Heart | Club | Diamond | Spade

type state = Setup | Pass | Play

(* [card] is the variant type of a card. *)
type card = {
  suit: suit;
  value: int;
}

(* [player_state] represents the state of the player. *)
type player_state = {
  hand: card list;
	round_points: int;
	game_points: int;
	ai_level: int;
	collected_cards: card list;
    p_num: int;
}

type game_state = {
    pool: card list;
	players: player_state list;
	phase: state;
    round_num: int;
}

type stored_data = {
  mutable has_clubs: bool list;
  mutable has_spades: bool list;
  mutable has_diamonds: bool list;
  mutable has_hearts: bool list;
  mutable q_spades_played: bool;
  mutable hearts_played: bool;
  mutable shooting_moon: bool;
  mutable tricks_p1: card list;
  mutable tricks_p2: card list;
  mutable tricks_p3: card list;
}

val get_index : (suit * int) -> card list -> int

val compare_cards : bool -> card -> card -> int

val compare_cards_with_suit : suit -> card -> card -> int
