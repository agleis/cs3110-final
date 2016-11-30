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
  pool: (card*int) list;
	prs: player_state list;
	phase: state;
  round_num: int;
}

type player_data = {
  mutable has_clubs: bool;
  mutable has_spades: bool;
  mutable has_diamonds: bool;
  mutable has_hearts: bool;
  mutable shooting_moon: bool;
  mutable tricks: card list;
  mutable round_points: int;
}

type stored_data = {
  mutable players: player_data list;
  mutable q_spades_played: bool;
  mutable hearts_played: bool;
}

val get_index : (suit * int) -> card list -> int

val compare_cards : bool -> card -> card -> int

val compare_cards_with_suit : suit -> card -> card -> int
