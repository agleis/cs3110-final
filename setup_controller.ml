open Types
open Sys
open Game_controller

let welcome_string = "Hello! Welcome to Hearts, a game of chance, skill, and "^
"lots of fun. We hope you enjoy!\n\n"

let continue = "Press Enter to continue.\n\n"

let rules_intro = "Here are some basic rules to get you started with the "^
"game:\n\n"

let rule_one = "1. You will be dealt 13 cards, and initially will be asked\n"^
"to select three of them to trade. These cards will be traded to your \n"^
"opponents, and you will receive cards from them as well.\n\n"

let rule_two = "2. During game play, each player will select one card to\n"^
"play into a central pool. The player who plays the first card into the pool\n"^
"determines the 'lead suit' - everyone else has to play this suit if they\n"^
"have it, and cards of this suit are ranked 'higher' than\n"^
"cards of any other suit (within a suit, cards are ranked by their value\n"^
"with Ace high).\n\n"

let rule_three = "3. Hearts cannot be played as the lead card until they\n"^
"have been 'broken' - that is, until someone has played a Heart in a\n"^
"previous round.\n\n"

let rule_four = "4. After all four cards are in the pool, the player with\n"^
"the highest ranked card takes it, and a new pool is played.\n\n"

let rule_five = "5. The goal of Hearts is to minimize the number of points\n"^
"you receive. You will receive points if you pick up a pool of cards that\n"^
"has Hearts in it - for instance, a pool with 2 Heart cards will net you\n"^
"two points, 3 Hearts will net you 3 points, etc. You will also receive\n"^
"points if you pick up the Queen of Spades - she is worth 13 points, so\n"^
"be careful!\n\n"

let rule_six = "6. However, there is a special case if you receive all \n"^
"of the Hearts and the Queen of Spades. This is called 'Shooting the Moon',\n"^
"and you will receive 0 points while your opponents each receive 26.\n"^
"This is pretty valuable, but easy to mess up - use wisely.\n\n"

let human_prompt = "Please enter the number of human players: "

let name_prompt = "Please enter a name for human number "

let ai_prompt = "Please enter the level (1 - 3) for the AI number "

let colon = ": "

let enter = "\n"

let ai_names = ["M. Clarkson"; "D. Gries"; "Chirag"; "M. George";
                "E. Tardos"; "R. Tate"; "A. Bracy"; "R. Constable";
                "J. Hopcroft"; "G. Morrisett"; "A. Myers"]
let rec lst_has_name lst name =
  match lst with
  | [] -> false
  | n::t -> if n = name then true else lst_has_name t name

let rec get_names num_humans human_number lst =
  match List.length lst with
  | 4 -> lst
  | _ -> if num_humans > 0
  then (print_string name_prompt; print_int human_number; print_string colon;
        let human_name = read_line () in
        let _ = Sys.command "clear" in
        get_names (num_humans - 1) (human_number + 1) (human_name::lst))
  else (Random.self_init ();
        let ai_length = List.length ai_names in
        let name_number = Random.int ai_length in
        let ai_name = List.nth ai_names name_number in
        if lst_has_name lst ai_name
        then get_names num_humans human_number lst
        else get_names num_humans human_number (ai_name::lst))

let rec get_ai_levels num_humans ai_num lst =
  match List.length lst with
  | 4 -> lst
  | _ -> if num_humans > 0
  then get_ai_levels (num_humans - 1) ai_num (0::lst)
  else (print_string ai_prompt; print_int ai_num; print_string colon;
       let ai_level = read_int () in
       let _ = Sys.command "clear" in
       get_ai_levels num_humans (ai_num + 1) (ai_level::lst))

let setup =
  print_string welcome_string;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rules_intro;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rule_one;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rule_two;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rule_three;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rule_four;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rule_five;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string rule_six;
  print_string continue;
  let _ = read_line () in
  let _ = Sys.command "clear" in
  print_string human_prompt;
  let num_humans = read_int () in
  let _ = Sys.command "clear" in
  let player_lst = get_ai_levels num_humans 1 [] in
  let names_lst = get_names num_humans 1 [] in
  main player_lst names_lst

let () = setup
