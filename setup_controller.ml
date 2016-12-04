open Types
open Game_controller

let welcome_string = "Hello! Welcome to Hearts, a game of chance, skill, and "^
"ruthless backstabbing. We hope you enjoy!\n"

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
       get_ai_levels num_humans (ai_num + 1) (ai_level::lst))

let setup =
  print_string welcome_string;
  print_string human_prompt;
  let num_humans = read_int () in
  print_string enter;
  let player_lst = get_ai_levels num_humans 1 [] in
  let names_lst = get_names num_humans 1 [] in
  main player_lst names_lst

let () = setup
