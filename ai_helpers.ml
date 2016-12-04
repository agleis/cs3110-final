open Types
open Randoms

type short_suit = SpadeS of int | ClubS of int | HeartS of int | DiamondS of int

type suit_values = {
  mutable spade: int list;
  mutable club: int list;
  mutable heart: int list;
  mutable diamond: int list;
}

let rec count_cards_of_suit suit hand i =
  match hand with
  | [] -> i
  | c::t -> if c.suit = suit
            then count_cards_of_suit suit t (i + 1)
            else count_cards_of_suit suit t i

let cards_of_suit suit hand =
  count_cards_of_suit suit hand 0

let hand_without_card hand card =
  List.filter (fun c -> c <> card) hand

let rec has_card_of_suit suit hand =
  match hand with
  | [] -> false
  | c::t -> if c.suit = suit then true else has_card_of_suit suit t

let is_early hand =
  List.length hand > 10

let rec has_cards = function
  | [] -> true
  | p::t -> p.has_clubs && p.has_spades && p.has_diamonds && p.has_hearts &&
            has_cards t

let rec shorted_suit = function
  | [] -> DiamondS (-1)
  | p::t -> if not p.has_clubs then ClubS 0
  else if not p.has_diamonds then DiamondS 0
  else if not p.has_spades then SpadeS 0
  else HeartS 0

let no_shorts data =
  has_cards data.players

let rec has_low_hearts hand =
  match hand with
  | [] -> false
  | c::t -> if c.suit = Heart && c.value <= 4
    then true
    else has_low_hearts t

let rec play_low_heart hand =
  let sorted_hand = List.sort (compare_cards false) hand in
  match sorted_hand with
  | [] -> random_card hand Heart
  | c::t -> if c.suit = Heart && c.value < 4
    then c
    else play_low_heart t

let get_shorted_suit data =
  shorted_suit data.players

let find_min_suit c s d h =
  if ((s = 0 || c <= s) &&
      (h = 0 || c <= h) &&
      (d = 0 || c <= d) &&
      c <> 0) then ClubS c
  else if ((s = 0 || d <= s) &&
          (h = 0 || d <= h) &&
          (c = 0 || d <= c) &&
          d <> 0) then DiamondS d
  else if ((s = 0 || h <= s) &&
          (c = 0 || h <= c) &&
          (d = 0 || h <= d) &&
          h <> 0) then HeartS h
  else SpadeS s

let find_max_suit c s d h =
  if (c >= s && c >= h && c >= d) then ClubS c
  else if (d >= c && d >= h && d >= s) then DiamondS d
  else if (h >= c && h >= d && h >= s) then HeartS h
  else SpadeS s

let rec get_num_suit_acc hand f c s d h =
  match hand with
  | [] -> f c s d h
  | card::t -> begin
    match card.suit with
    | Club -> get_num_suit_acc t f (c + 1) s d h
    | Spade -> get_num_suit_acc t f c (s + 1) d h
    | Diamond -> get_num_suit_acc t f c s (d + 1) h
    | Heart -> get_num_suit_acc t f c s d (h + 1)
  end

let get_short_suit hand =
  get_num_suit_acc hand find_min_suit 0 0 0 0

let get_long_suit hand =
  get_num_suit_acc hand find_max_suit 0 0 0 0

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
               value = List.nth (List.sort (Pervasives.compare) (h::t)) (List.length (h::t) - 1)}
  end
  | Club -> begin
    match suit_ind.club with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Club;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) (List.length (h::t) - 1)}
  end
  | Spade -> begin
    match suit_ind.spade with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Spade;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) (List.length (h::t) - 1)}
  end
  | Diamond -> begin
    match suit_ind.diamond with
    | [] -> get_random_not_two hand ind1 ind2
    | h::t -> {suit = Diamond;
               value = List.nth (List.sort (Pervasives.compare) (h::t)) (List.length (h::t) - 1)}
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

let low_card_from_suit hand suit ind1 ind2 =
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

let low_card_from_suit_not hand suit ind1 ind2 =
  let suit_ind = get_suit_values hand in
  let num_hearts = List.length (suit_ind.heart) in
  let num_clubs = List.length (suit_ind.club) in
  let num_diamonds = List.length (suit_ind.diamond) in
  let num_spades = List.length (suit_ind.spade) in
  let max_suit = (match suit with
  | Heart -> find_max_suit num_clubs num_spades num_diamonds 0
  | Diamond -> find_max_suit num_clubs num_spades 0 num_hearts
  | Spade -> find_max_suit num_clubs 0 num_diamonds num_hearts
  | Club -> find_max_suit 0 num_spades num_diamonds num_hearts) in
  match max_suit with
  | HeartS _ -> low_card_from_suit hand Heart ind1 ind2
  | ClubS _ -> low_card_from_suit hand Club ind1 ind2
  | SpadeS _ -> low_card_from_suit hand Spade ind1 ind2
  | DiamondS _ -> low_card_from_suit hand Diamond ind1 ind2

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

let highest_card_so_far pool lead_suit =
  match pool with
  | [] -> {suit = Diamond; value = -1}
  | c1::[] -> c1
  | c1::c2::[] -> if compare_cards_with_suit lead_suit c1 c2 >= 0
                  then c1
                  else c2
  | c1::c2::c3::[] -> if compare_cards_with_suit lead_suit c1 c2 >= 0
                      then if compare_cards_with_suit lead_suit c1 c3 >= 0
                           then c1
                           else c3
                      else if compare_cards_with_suit lead_suit c2 c3 >= 0
                           then c2
                           else c3
  | c::t -> c

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

let screw_other_player hand lead_suit =
  if has_card_of_suit lead_suit hand
  then low_card_from_suit hand lead_suit (-1) (-1)
  else if get_index (Spade, 12) hand >= 0
  then {suit = Spade; value = 12}
  else high_card_from_suit hand Heart (-1) (-1)

let rec get_losing_card card hand =
  let possible_heart = high_card_from_suit hand Heart (-1) (-1) in
  if possible_heart.value = (-1)
  then match hand with
  | [] -> {suit = Diamond; value = -1}
  | c::t -> if compare_cards_with_suit card.suit c card < 0
            then c
            else get_losing_card card t
  else possible_heart

let rec highest_loser card hand max_val =
  let cards_of_suit = List.filter (fun c -> c.suit = card.suit) hand in
  match cards_of_suit with
  | [] -> {suit = card.suit; value = max_val}
  | c::t -> if c.value < card.value && c.value > max_val
            then highest_loser card t c.value
            else highest_loser card t max_val

let highest_losing_card card hand h_played =
  if has_card_of_suit card.suit hand
  then highest_loser card hand (-1)
  else screw_other_player hand card.suit

let rec player_cards_of_suit suit players lst =
  match players with
  | [] -> lst
  | p::t -> let new_lst = List.filter (fun c -> c.suit = suit) p.tricks in
    player_cards_of_suit suit t (new_lst@lst)

let cards_played suit pool data =
  let pool_cards = List.filter (fun c -> c.suit = suit) pool in
  let player_cards = player_cards_of_suit suit data.players [] in
  player_cards@pool_cards

let rec build_list suit i lst =
  if i >= 2
  then build_list suit (i - 1) ({suit = suit; value = i}::lst)
  else lst

let rec not_in_lst c = function
  | [] -> true
  | c1::t -> if c1 = c then false else not_in_lst c t

let inverse_list suit lst =
  let full_suit = build_list suit 14 [] in
  List.filter (fun c -> not_in_lst c lst) full_suit

let beating_cards_still_out card pool data =
  let high_suit = card.suit in
  let cards_of_suit_played = cards_played high_suit pool data in
  inverse_list card.suit cards_of_suit_played

let lowest_beating_card lst hand h_played =
  let sorted_beaters = List.sort (compare_cards false) lst in
  let sorted_hand = List.sort (compare_cards true) lst in
  let highest_beater =
    try List.hd sorted_beaters with
    | _ -> {suit = Diamond; value = -1} in
  let beater_mine = beat_card highest_beater sorted_hand in
  if beater_mine.value = (-1)
  then high_card_from_suit sorted_hand highest_beater.suit (-1) (-1)
  else beater_mine

let lose_to_card card hand =
  if has_card_of_suit card.suit hand
  then low_card_from_suit hand card.suit (-1) (-1)
  else if card.suit = Heart
  then if can_lose card hand
       then get_losing_card card hand
       else {suit = Diamond; value = -1}
  else screw_other_player hand card.suit

let rec contains_heart_or_q_spades pool =
  match pool with
  | [] -> false
  | c::t -> if c.suit = Heart || (c.suit = Spade && c.value = 12)
            then true
            else contains_heart_or_q_spades t

let rec count_falses lst suit num =
  match lst with
  | [] -> num
  | p::t -> begin
    match suit with
    | Club -> if p.has_clubs
              then count_falses t suit num
              else count_falses t suit (num + 1)
    | Heart -> if p.has_hearts
              then count_falses t suit num
              else count_falses t suit (num + 1)
    | Diamond -> if p.has_diamonds
              then count_falses t suit num
              else count_falses t suit (num + 1)
    | Spade -> if p.has_spades
              then count_falses t suit num
              else count_falses t suit (num + 1)
  end

let count_shorted suit data =
  count_falses data.players suit 0

let check_if_shorted suit data =
  if count_shorted suit data >= 2
  then ((List.nth data.players 0).shooting_moon <- true; data)
  else data

let rec no_points player =
  player.round_points = 0

let rec no_player_points players p_num =
  match p_num with
  | 0 -> no_points (List.nth players 1) &&
         no_points (List.nth players 2) &&
         no_points (List.nth players 3)
  | 1 -> no_points (List.nth players 0) &&
         no_points (List.nth players 2) &&
         no_points (List.nth players 3)
  | 2 -> no_points (List.nth players 1) &&
         no_points (List.nth players 0) &&
         no_points (List.nth players 3)
  | _ -> no_points (List.nth players 1) &&
         no_points (List.nth players 2) &&
         no_points (List.nth players 0)

let check_if_shooting_moon hand pool data p_num =
  if no_player_points data.players p_num
  then match get_long_suit hand with
  | ClubS x -> if x >= (List.length hand)/2
               then check_if_shorted Club data else data
  | HeartS x -> if x >= (List.length hand)/2
                then check_if_shorted Heart data else data
  | DiamondS x -> if x >= (List.length hand)/2
                  then check_if_shorted Diamond data else data
  | SpadeS x -> if x >= (List.length hand)/2
                then check_if_shorted Spade data else data
  else ((List.nth data.players 0).shooting_moon <- false; data)
