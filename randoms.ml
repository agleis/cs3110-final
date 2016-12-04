open Types

let rec has_card_of_suit suit hand =
  match hand with
  | [] -> false
  | c::t -> if c.suit = suit then true else has_card_of_suit suit t

let rec get_random_not_two hand ind1 ind2 =
  if List.length hand = 0
  then {suit = Diamond; value = -1}
  else (Random.self_init ();
  let rand = Random.int (List.length hand) in
  if (rand = ind1 || rand = ind2)
  then get_random_not_two hand ind1 ind2
  else List.nth hand rand)

let rec get_two_random_not hand ind =
  if List.length hand = 0
  then []
  else (Random.self_init ();
  let rand1 = Random.int (List.length hand) in
  let rand2 = Random.int (List.length hand) in
  if rand1 = ind || rand2 = ind || rand1 = rand2
  then get_two_random_not hand ind
  else (List.nth hand rand1)::(List.nth hand rand2)::[])

let rec get_three_random hand =
  if List.length hand = 0
  then []
  else (Random.self_init ();
  let rand1 = Random.int (List.length hand) in
  let rand2 = Random.int (List.length hand) in
  let rand3 = Random.int (List.length hand) in
  if rand1 = rand2 || rand2 = rand3 || rand1 = rand3
  then get_three_random hand
  else [List.nth hand rand1;
        List.nth hand rand2;
        List.nth hand rand3])

let rec random_card hand lead_suit =
  if List.length hand = 0
  then {suit = Diamond; value = -1}
  else (Random.self_init ();
  if has_card_of_suit lead_suit hand
  then let card = List.nth hand (Random.int (List.length hand)) in
    if card.suit <> lead_suit
    then random_card hand lead_suit
    else card
  else List.nth hand (Random.int (List.length hand)))

let rec random_card_no_suit hand =
  if List.length hand = 0
  then {suit = Diamond; value = -1}
  else (Random.self_init ();
    let card = List.nth hand (Random.int (List.length hand)) in
    if card.suit = Heart &&
       (has_card_of_suit Club hand || 
        has_card_of_suit Diamond hand ||
        has_card_of_suit Spade hand)
    then random_card_no_suit hand
    else card)
