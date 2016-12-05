open Graphics
open Types
open Graphics_helper
(* #load "cma" *)

(******************************************************************************)
(** Global Variables **********************************************************)
(******************************************************************************)

let window_width = 1280
let window_height = 750
let card_height = 90
let card_width = 60
let card_spacing = 5
let exit = ref true
let player_hand = ref []

(******************************************************************************)
(** End Global Variables ******************************************************)
(******************************************************************************)


(* 
 * [x_triple a] function similar to fst and snd
 * for tuples, but instead for triples. If x is
 * f then gets first in triple, s gets second,
 * and t gets third.
*)
let f_triple (a,_,_) = a
let s_triple (_,a,_) = a
let t_triple (_,_,a) = a


(* 
 * [draw_string1 s x y] Similar to draw_string for
 * the graphics module but instead uses the letters
 * defined in graphics_helper. Draws the string [s]
 * at position x y on the screen.
*)
let draw_string1 s x y =
  let lst = explode s in
  let () = set_line_width 10 in
  for i = 0 to (List.length lst) - 1 do
    if (List.nth lst i) = ' ' then moveto (x+(50*i)) y else
    draw_letter (List.nth lst i) (x + (50*i)) y
  done

let clear_player_hand () =
  player_hand := []

let get_player_hand () =
  !player_hand

let init_window w h =
  let s = " " ^ (string_of_int w) ^ "x" ^ (string_of_int h) in
  open_graph s


(******************************************************************************)
(** Main Draw Functions *******************************************************)
(******************************************************************************)


let draw_card num suit x y =
    let card_char =
      match num with
      |11 -> "J"
      |12 -> "Q"
      |13 -> "K"
      |14 -> "A"
      |0 -> "V"
      |(-1) -> "H"
      |_ -> string_of_int num
    in
    let () =
    if card_char = "V" then
      begin
        set_color black;
        fill_rect x y card_width card_height;
        set_color white;
        fill_rect (x+2) (y+2) (card_width - 4) (card_height - 4);
        set_color red;
        fill_rect (x+5) (y+5) (card_width - 10) (card_height - 10)
      end
    else if card_char = "H" then
      begin
        set_color black;
        fill_rect x y card_height card_width;
        set_color white;
        fill_rect (x+2) (y+2) (card_height - 4) (card_width - 4);
        set_color red;
        fill_rect (x+5) (y+5) (card_height - 10) (card_width - 10)
      end
    else
      begin
        set_color black;
        fill_rect x y card_width card_height;
        set_color white;
        fill_rect (x+2) (y+2) (card_width - 4) (card_height - 4);
        if suit = Heart then
          begin
            let z = if num == 10 then 45 else 50 in
            set_color red;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+z) (y+10);
            draw_string card_char;
            moveto (x+ (int_of_float ((float card_width)/.6.0))) (y+40);
            draw_symbol Heart (x+(card_width/2)) (y+(card_height/2))
          end
        else if suit = Diamond then
          begin
            let z = if num == 10 then 45 else 50 in
            set_color red;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+z) (y+10);
            draw_string card_char;
            moveto (x+ (int_of_float ((float card_width)/.6.0))) (y+40);
            draw_symbol Diamond (x+(card_width/2)) (y+(card_height/2))
          end
        else if suit = Spade then
          begin
            let z = if num == 10 then 45 else 50 in
            set_color black;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+z) (y+10);
            draw_string card_char;
            moveto (x+10) (y+40);
            draw_symbol Spade (x+(card_width/2)) (y+(card_height/2))
          end
        else
          begin
            let z = if num == 10 then 45 else 50 in
            set_color black;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+z) (y+10);
            draw_string card_char;
            moveto (x+10) (y+40);
            draw_symbol Club (x+(card_width/2)) (y+(card_height/2))
          end
      end
    in
    ()

let draw_hand lst s =
  clear_player_hand ();
  set_color black;
  moveto ((window_width/2)-125) ((window_height/2)-225);
  draw_string ((f_triple s) ^ "         " ^ (s_triple s) ^ "         " ^ (t_triple s));
  let delta = ref 0 in
  let len = List.length lst in
  let total_len = (len * (card_width + card_spacing)) in
  let () =
  for i=0 to (List.length lst) - 1 do
      let xpos = ((int_of_float (0.5*.(float window_width))) + !delta - (total_len/2)) in
      let ypos = (int_of_float (0.06*.(float window_height))) in
      let key =
        if i = 0 then "~"
        else if i = 10 then "0"
        else if i = 11 then "-"
        else if i = 12 then "="
        else string_of_int i
      in
      draw_card ((List.nth lst i).value) ((List.nth lst i).suit) xpos ypos;
      moveto (xpos+(card_width/2)) (ypos-20);
      set_color black;
      draw_string key;
      player_hand := !player_hand@[(List.nth lst i,xpos,ypos)];
      delta := !delta + (card_width + card_spacing);
    done;
  in ()

let draw_card_top num x y s =
  let () =
  let delta = ref 0 in
  set_color black;
  moveto ((x-(int_of_float (0.05*.(float window_width)))) - 40) (y+(int_of_float (0.1*.(float window_height))));
  draw_string (f_triple s);
  moveto ((x-(int_of_float (0.05*.(float window_width)))) - 40) ((y+(int_of_float (0.1*.(float window_height)))) - 20);
  draw_string (s_triple s);
  moveto ((x-(int_of_float (0.05*.(float window_width)))) - 40) ((y+(int_of_float (0.1*.(float window_height)))) - 40);
  draw_string (t_triple s);
  for i=1 to num do
    draw_card 0 Spade (x + !delta) y;
    delta := !delta + (int_of_float ((float window_width)*.0.03))
  done;
  in ()

let draw_card_side num x y s side =
  let () =
  let delta = ref 0 in
  let () =
    if side then
      begin
        set_color black;
        moveto (x-(int_of_float (0.08*.(float window_width)))) ((y+(int_of_float (0.5*.(float window_height)))) - 0);
        draw_string (f_triple s);
        moveto (x-(int_of_float (0.08*.(float window_width)))) ((y+(int_of_float (0.5*.(float window_height)))) - 20);
        draw_string (s_triple s);
        moveto (x-(int_of_float (0.08*.(float window_width)))) ((y+(int_of_float (0.5*.(float window_height)))) - 40);
        draw_string (t_triple s)
      end
    else
      begin
        set_color black;
        moveto (x+(int_of_float (0.08*.(float window_width)))) ((y+(int_of_float (0.5*.(float window_height)))) - 0);
        draw_string (f_triple s);
        moveto (x+(int_of_float (0.08*.(float window_width)))) ((y+(int_of_float (0.5*.(float window_height)))) - 20);
        draw_string (s_triple s);
        moveto (x+(int_of_float (0.08*.(float window_width)))) ((y+(int_of_float (0.5*.(float window_height)))) - 40);
        draw_string (t_triple s)
      end
  in
  for i=1 to num do
    draw_card (-1) Spade x (y + !delta);
    delta := !delta + (int_of_float ((float window_height)*.0.05))
  done;
  in ()

let draw_pool state pool =
  let delta = ref 0 in
  let len = List.length pool in
  let total_len = (len * (card_width + card_spacing)) in
  let () =
  for i=0 to (List.length pool) - 1 do
    let xpos = ((int_of_float (0.5*.(float window_width))) + !delta - (total_len/2)) in
    let ypos = (int_of_float (0.45*.(float window_height))) in
    draw_card ((fst (List.nth pool i)).value) ((fst (List.nth pool i)).suit) xpos ypos;
    moveto xpos (ypos - 15);
    set_color black;
    let player = (List.nth (List.filter (fun x -> x.p_num = (snd (List.nth pool i))) state.prs) 0).name in 
    draw_string player;
    delta := !delta + (card_width + card_spacing + 20);
  done;
  in ()

let draw_play_phase x y =
  set_line_width 10;
  set_color black;
  draw_string1 "YOUR TURN" x y

let draw_pass_phase round x y =
  set_line_width 10;
  set_color black;
  draw_string1 "PASS THREE" x y;
    (match round with
    | 0 -> draw_left_arrow (x+300) (y+100)
    | 1 -> draw_right_arrow (x+300) (y+100) 
    | 2 -> draw_across_arrow (x+300) (y+100)
    | _ -> ())

let draw_phase phase round_num x y =
  match phase with
  |Play -> draw_play_phase (x+60) y
  |Pass -> draw_pass_phase (round_num mod 4) (x-50) (y-300)
  |Setup -> draw_pass_phase (round_num mod 4) x (y-300)

let rec switch_player pl =
  clear_graph ();
  set_color black;
  moveto (window_width/2) (window_height/2);
  draw_string1 "PRESS ENTER TO" ((window_width/2) - 350) (window_height/2);
  draw_string1 ("SWITCH TO " ^ pl) ((window_width/2) - 350) ((window_height/2) - 60);
  let s = wait_next_event [Key_pressed] in
  if s.keypressed && s.key = '\r' then () else switch_player pl

let rec winner state pnum =
  clear_graph ();
  set_color black;
  draw_string1 " HAND GOES TO " ((window_width/2) - 350) ((window_height/2) + 160);
  let player = (List.nth (List.filter (fun x -> x.p_num = pnum) state.prs) 0).name in 
  draw_string1 player ((window_width/2) - 200) ((window_height/2) + 100);
  draw_string1 "PRESS ENTER TO CONTINUE" ((window_width/2) - 575) ((window_height/2) - 200);
  draw_pool state state.pool;
  let s = wait_next_event [Key_pressed] in
  if s.keypressed && s.key = '\r' then () else winner state  pnum

let rec game_points plst =
  clear_graph ();
  set_color black;
  for i = 0 to (List.length plst) - 1 do
    let player = (List.nth (List.filter (fun x -> x.p_num = i) plst) 0).name in 
    let points = string_of_int ((List.nth (List.filter (fun x -> x.p_num = i) plst) 0).round_pts) in 
    draw_string1 (player ^ ": " ^ points) ((window_width/2)-525) (((3*(window_height/4)) - (i*60)))
  done;
  draw_string1 "PRESS ENTER TO CONTINUE" ((window_width/2) - 575) (80);
  let s = wait_next_event [Key_pressed] in
  if s.keypressed && s.key = '\r' then () else game_points plst

let rec draw_end_game lst =
  clear_graph ();
  set_color black;

  let min = ref (200) in 
  let pl = ref " " in 

  for i = 0 to (List.length lst) - 1 do
    let () = 
    if (List.nth lst i).game_points < !min then
    begin
    min := (List.nth lst i).game_points;
    pl := (List.nth lst i).name
    end
    else () 
    in 
    draw_string1 ((List.nth lst i).name ^ ": " ^ (string_of_int ((List.nth lst i).game_points))) ((window_width/2)-525) (((3*(window_height/4)) - (i*60)))
  done;
  draw_string1 ("WINNER IS " ^ !pl) 120 200; 
  draw_string1 "PRESS ENTER TO CONTINUE" ((window_width/2) - 575) (80);
  let s = wait_next_event [Key_pressed] in
  if s.keypressed && s.key = '\r' then () else draw_end_game lst 

let player_string state idx =
  let player = (List.nth state.prs idx).name in 
  let game_points = "Game Points: " ^ (string_of_int (List.nth state.prs idx).game_points) in
  let round_points = "Round Points: " ^ (string_of_int (List.nth state.prs idx).round_pts) in
  (player, game_points, round_points)

let rec find_index lst num acc =
  match lst with
  |[] -> acc
  |h::t -> if h.p_num == num then acc else find_index t num (acc+1)

let draw_board state pstate =
  init_window window_width window_height;
  set_window_title "CS 3110 Hearts Game";
  clear_graph ();
  let human_index = find_index state.prs state.last_human_player 0 in
  let human_pstate = List.nth state.prs human_index in
  let left_index = (human_index + 1) mod 4 in
  let top_index = (human_index + 2) mod 4 in
  let right_index = (human_index + 3) mod 4 in
  let num_left = List.length ((List.nth state.prs left_index).hand) in
  let num_right = List.length ((List.nth state.prs right_index).hand) in
  let num_top = List.length ((List.nth state.prs top_index).hand) in
  if (state.last_human_player = pstate.p_num) then
  draw_phase state.phase state.round_num (int_of_float (0.3*.(float window_width))) (int_of_float (0.65*.(float window_height)));
  draw_card_top num_top ((int_of_float (0.30*.(float window_width)))) (int_of_float (0.8*.(float window_height))) (player_string state top_index);
  draw_card_side num_left (int_of_float (0.095*.(float window_width))) ((int_of_float (0.20*.(float window_height)))) (player_string state left_index) true;
  draw_card_side num_right ((int_of_float (0.905*.(float window_width))) - card_height) ((int_of_float (0.20*.(float window_height)))) (player_string state right_index) false;
  draw_pool state state.pool;
  draw_hand human_pstate.hand (player_string state human_index)

let rec find_pos ph c =
  match ph with
  |[] -> (-1,-1)
  |h::t -> if (f_triple h) = c then (s_triple h, t_triple h) else find_pos t c

let card_selection st pstate lst ph =
  clear_graph ();
  draw_board st pstate;
  if List.length lst = 0 then () else
    begin
      for i=0 to (List.length lst) - 1 do
        let c = List.nth lst i in
        let () = set_line_width 10 in
        let () = set_color yellow in
        let new_pos = find_pos ph c in
        draw_rect (fst new_pos) (snd new_pos) card_width card_height
      done
    end

let rec wait_for_enter () =
  let s = wait_next_event [Key_pressed] in
  if (s.keypressed && s.key = '\r') then () else wait_for_enter ()

let new_lst c lst num =
  if List.mem c lst  then
    List.filter (fun x -> x <> c) lst
  else
    begin
      if List.length lst > num then lst else c::lst
    end

let index c x = 
  match c with
  |'`' ->  if x > 0 then 0 else (-2)
  |'1' ->  if x > 1 then 1 else (-2)
  |'2' ->  if x > 2 then 2 else (-2)
  |'3' ->  if x > 3 then 3 else (-2)
  |'4' ->  if x > 4 then 4 else (-2)
  |'5' ->  if x > 5 then 5 else (-2)
  |'6' ->  if x > 6 then 6 else (-2)
  |'7' ->  if x > 7 then 7 else (-2)
  |'8' ->  if x > 8 then 8 else (-2)
  |'9' ->  if x > 9 then 9 else (-2)
  |'0' ->  if x > 10 then 10 else (-2)
  |'-' ->  if x > 11 then 11 else (-2)
  |'=' ->  if x > 12 then 12 else (-2)
  |'\r' -> (-2)
  |_ -> (-2)


let rec click_card lst st pstate =
  let s = wait_next_event [Key_pressed] in
  let ph = get_player_hand () in 
  let current_hand_len = List.length ph in 
  let c x = f_triple (List.nth ph x) in 
  let idx = index s.key current_hand_len in 
  if idx = (-2) then 
    begin
      if List.length lst > 0 then List.hd lst
      else click_card lst st pstate
    end
  else
    begin
      let n_lst = new_lst (c idx) lst 0 in
      let () = card_selection st pstate n_lst ph in
      click_card n_lst st pstate
    end

let rec trade_cards lst st pstate =
  let s = wait_next_event [Key_pressed] in
  let ph = get_player_hand () in 
  let current_hand_len = List.length ph in 
  let c x = f_triple (List.nth ph x) in 
  let idx = index s.key current_hand_len in 
  if idx = (-2) then 
    begin
      if List.length lst > 2 then lst
      else trade_cards lst st pstate
    end
  else
    begin
      let n_lst = new_lst (c idx) lst 2 in
      let () = card_selection st pstate n_lst ph in
      trade_cards n_lst st pstate 
    end


