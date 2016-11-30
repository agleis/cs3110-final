open Types
open Game_controller

let welcome_string = "Hello! Welcome to Hearts, a game of chance, skill, and "^
"ruthless backstabbing. We hope you enjoy!\n"

let human_prompt = "Please enter the number of human players: "

let ai_prompt = "Please enter the level for the AI number "

let colon = ": "

let enter = "\n"

let rec get_ai_levels num_humans ai_num lst =
  match List.length lst with
  | 4 -> lst
  | _ -> if num_humans > 0
  then get_ai_levels (num_humans - 1) ai_num (0::lst)
  else (print_string ai_prompt; print_int ai_num; print_string colon;
       let ai_level = read_int () in
       get_ai_levels num_humans (ai_num + 1) (ai_level::lst))

let setup =
  print_string welcome_string;
  print_string human_prompt;
  let num_humans = read_int () in
  print_string enter;
  let player_lst = get_ai_levels num_humans 1 [] in
  main player_lst

let () = setup
