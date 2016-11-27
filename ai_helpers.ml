open Types
open Randoms

type short_suit = SpadeS of int | ClubS of int | HeartS of int | DiamondS of int

type suit_values = {
  mutable spade: int list;
  mutable club: int list;
  mutable heart: int list;
  mutable diamond: int list;
}

let hand_without_card hand card =
  List.filter (fun c -> c <> card) hand

let is_early hand =
  List.length hand > 10

let has_cards lst =
  match lst with
  | [true;true;true] -> true
  | _ -> false

let no_shorts data =
  has_cards data.has_clubs &&
  has_cards data.has_diamonds &&
  has_cards data.has_hearts &&
  has_cards data.has_spades

let rec has_low_hearts hand =
  match hand with
  | [] -> false
  | c::t -> if c.suit = Heart && c.value <= 4
    then true
    else has_low_hearts t

let rec play_low_heart hand =
  let sorted_hand = List.sort (compare_cards false) hand in
  match sorted_hand with
  | [] -> {suit = Diamond; value = -1}
  | c::t -> if c.suit = Heart && c.value < 4
    then c
    else play_low_heart t

let get_shorted_suit data =
  if not (has_cards data.has_clubs)
  then ClubS 0
  else if not (has_cards data.has_diamonds)
  then DiamondS 0
  else if not (has_cards data.has_hearts)
  then HeartS 0
  else SpadeS 0

let find_min_suit c s d h =
  if (c <= s && c <= h && c <= d) then ClubS c
  else if (d <= c && d <= h && d <= s) then DiamondS d
  else if (h <= c && h <= d && h <= s) then HeartS h
  else SpadeS s

let rec get_short_suit_acc hand c s d h =
  match hand with
  | [] -> find_min_suit c s d h
  | card::t -> begin
    match card.suit with
    | Club -> get_short_suit_acc t (c + 1) s d h
    | Spade -> get_short_suit_acc t c (s + 1) d h
    | Diamond -> get_short_suit_acc t c s (d + 1) h
    | Heart -> get_short_suit_acc t c s d (h + 1)
  end

let get_short_suit hand =
  get_short_suit_acc hand 0 0 0 0

let rec rec_get_suit_values hand record =
  match hand with
  | [] -> record
  | h::t -> begin
    match h.suit with
    | Heart -> record.heart <- (h.value)::record.heart;
               rec_get_suit_values t record
    | Club -> record.club <- (h.value)::record.club;
              rec_get_suit_values t record
    | Spade -> record.spade <- (h.value)::record.spade;
               rec_get_suit_values t record
    | Diamond -> record.diamond <-(h.value)::record.diamond;
                 rec_get_suit_values t record
  end

let get_suit_values hand =
  let initial_suit_values = {
    spade = []; heart = []; club = []; diamond = [];
  } in
  rec_get_suit_values hand initial_suit_values

let high_card_from_suit hand suit ind1 ind2 =
  let suit_ind = get_suit_values hand in
  match suit with
  | Heart -> begin
    match suit_ind.heart with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Heart;
               value = List.hd (List.sort (Pervasives.compare) (h::t))}
  end
  | Club -> begin
    match suit_ind.club with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Club;
               value = List.hd (List.sort (Pervasives.compare) (h::t))}
  end
  | Spade -> begin
    match suit_ind.spade with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Spade;
               value = List.hd (List.sort (Pervasives.compare) (h::t))}
  end
  | Diamond -> begin
    match suit_ind.diamond with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Diamond;
               value = List.hd (List.sort (Pervasives.compare) (h::t))}
  end

let middle_card_from_suit hand suit ind1 ind2 =
  let suit_ind = get_suit_values hand in
  match suit with
  | Heart -> begin
    match suit_ind.heart with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Heart;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) ((List.length (h::t)) / 2)}
  end
  | Club -> begin
    match suit_ind.club with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Club;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) ((List.length (h::t)) / 2)}
  end
  | Spade -> begin
    match suit_ind.spade with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Spade;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) ((List.length (h::t)) / 2)}
  end
  | Diamond -> begin
    match suit_ind.diamond with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Diamond;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) ((List.length (h::t)) / 2)}
  end

let high_card_from_suit_not hand suit ind1 ind2 =
  let suit_ind = get_suit_values hand in
  let num_hearts = List.length (suit_ind.heart) in
  let num_clubs = List.length (suit_ind.club) in
  let num_diamonds = List.length (suit_ind.diamond) in
  let num_spades = List.length (suit_ind.spade) in
  let min_suit = (match suit with
  | Heart -> find_min_suit num_clubs num_spades num_diamonds 0
  | Diamond -> find_min_suit num_clubs num_spades 0 num_hearts
  | Spade -> find_min_suit num_clubs 0 num_diamonds num_hearts
  | Club -> find_min_suit 0 num_spades num_diamonds num_hearts) in
  match min_suit with
  | HeartS _ -> high_card_from_suit hand Heart ind1 ind2
  | ClubS _ -> high_card_from_suit hand Club ind1 ind2
  | SpadeS _ -> high_card_from_suit hand Spade ind1 ind2
  | DiamondS _ -> high_card_from_suit hand Diamond ind1 ind2

let high_card_from_short_suit hand ind1 ind2 =
  match get_short_suit hand with
  | HeartS _ -> high_card_from_suit hand Heart ind1 ind2
  | ClubS _ -> high_card_from_suit hand Club ind1 ind2
  | SpadeS _ -> high_card_from_suit hand Spade ind1 ind2
  | DiamondS _ -> high_card_from_suit hand Diamond ind1 ind2

let middle_card_from_short_suit hand =
  match get_short_suit hand with
  | HeartS _ -> middle_card_from_suit hand Heart (-1) (-1)
  | ClubS _ -> middle_card_from_suit hand Club (-1) (-1)
  | SpadeS _ -> middle_card_from_suit hand Spade (-1) (-1)
  | DiamondS _ -> middle_card_from_suit hand Diamond (-1) (-1)

let highest_card_so_far pool =
  match pool with
  | [] -> {suit = Diamond; value = -1}
  | c1::[] -> c1
  | c1::c2::[] -> if compare_cards_with_suit c1.suit c1 c2 >= 0
                  then c1
                  else c2
  | c1::c2::c3::[] -> if compare_cards_with_suit c1.suit c1 c2 >= 0
                      then if compare_cards_with_suit c1.suit c1 c3 >= 0
                           then c1
                           else c3
                      else if compare_cards_with_suit c1.suit c2 c3 >= 0
                           then c2
                           else c3

let rec beat_card card hand =
  match hand with
  | [] -> {suit = Diamond; value = -1}
  | c::t -> if compare_cards_with_suit card.suit c card > 0
            then c
            else beat_card card t

let rec can_lose card hand =
  match hand with
  | [] -> false
  | c::t -> if compare_cards_with_suit card.suit c card < 0
            then true
            else can_lose card t

let rec get_losing_card card hand =
  let possible_heart = high_card_from_suit hand Heart (-1) (-1) in
  if possible_heart.value = (-1)
  then match hand with
  | [] -> {suit = Diamond; value = -1}
  | c::t -> if compare_cards_with_suit card.suit c card < 0
            then c
            else get_losing_card card t
  else possible_heart

let lose_to_card card hand =
  if card.suit = Heart
  then if can_lose card hand
       then get_losing_card card hand
       else high_card_from_suit_not hand Heart (-1) (-1)
  else if can_lose card hand
       then get_losing_card card hand
       else high_card_from_suit_not hand Heart (-1) (-1)
