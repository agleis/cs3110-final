let rec get_random_not_two hand ind1 ind2 =
  Random.self_init ();
  let rand = Random.int (List.length hand) in
  if (rand = ind1 || rand = ind2)
  then get_random_not_two hand ind1 ind2
  else List.nth hand rand

let rec get_two_random_not hand ind =
  Random.self_init ();
  let rand1 = Random.int (List.length hand) in
  let rand2 = Random.int (List.length hand) in
  if rand1 = ind || rand2 = ind
  then get_two_random_not hand ind
  else (List.nth hand rand1)::(List.nth hand rand2)::[]

let get_three_random hand =
  Random.self_init ();
  [List.nth hand (Random.int (List.length hand));
   List.nth hand (Random.int (List.length hand));
   List.nth hand (Random.int (List.length hand))]

let random_card hand =
  Random.self_init ();
  List.nth hand (Random.int (List.length hand))
