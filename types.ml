type suit = Heart | Club | Diamond | Spade

type state = Setup | Pass | Play

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

let compare_desc i1 i2 =
  if i1 < i2
  then 1
  else if i1 = i2
       then 0
       else -1

let rec rec_get_index (s, v) hand acc =
  match hand with
  | [] -> -1
  | h::t -> (if h.suit = s && h.value = v
             then acc
             else rec_get_index (s, v) t (acc + 1))

let get_index (s, v) hand =
  rec_get_index (s, v) hand 0

let compare_cards asc c1 c2 =
  if asc
  then if c1.suit = c2.suit
       then if c2.value < c1.value
            then 1
            else if c2.value = c1.value
                 then 0
                 else -1
       else if (c2.suit = Club ||
            (c2.suit = Diamond && c1.suit != Club) ||
            (c2.suit = Heart && c1.suit = Spade))
            then 1
            else -1
  else if c1.suit = c2.suit
       then if c1.value < c2.value
            then 1
            else if c1.value = c2.value
                 then 0
                 else -1
       else if (c1.suit = Club ||
            (c1.suit = Diamond && c2.suit != Club) ||
            (c1.suit = Heart && c2.suit = Spade))
            then 1
            else -1

let compare_cards_with_suit suit c1 c2 =
  if c1.suit = suit && c2.suit <> suit
  then 1
  else if c2.suit = suit && c1.suit <> suit
  then -1
  else if c1.value > c2.value
  then 1
  else if c1.value = c2.value
  then 0
  else -1
