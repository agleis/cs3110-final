open Graphics
open Types
(* #load "cma" *)

exception End;;

let lst1 = [{suit=Heart; value=1};{suit=Heart; value=2};{suit=Club; value=3};{suit=Club; value=4};{suit=Diamond; value=5};{suit=Diamond; value=6}; {suit=Spade; value=7}; {suit=Spade; value=8}; {suit=Heart; value=9}; {suit=Club; value=10};{suit=Diamond; value=11};{suit=Spade; value=12};{suit=Heart; value=13}]
let lst2 = [{suit=Heart; value=1};{suit=Heart; value=2};{suit=Club; value=3};{suit=Club; value=4};{suit=Diamond; value=5};{suit=Diamond; value=6}]
let lst3 = [{suit=Spade; value=1};{suit=Spade; value=2};{suit=Spade; value=3};{suit=Spade; value=4};{suit=Spade; value=5};{suit=Spade; value=6}; {suit=Spade; value=7}; {suit=Spade; value=8}; {suit=Spade; value=9}; {suit=Spade; value=10};{suit=Spade; value=11};{suit=Spade; value=12};{suit=Spade; value=13}]
let lst4 = [{suit=Heart; value=7};{suit=Heart; value=8};{suit=Club; value=9};{suit=Club; value=10};{suit=Diamond; value=11};{suit=Diamond; value=12}]
let pool1 = [({suit=Diamond; value=5},2); ({suit=Diamond; value=6},3); ({suit=Spade; value=7},4); ({suit=Spade; value=8},1)]

let player_state1 = {
  hand = lst1;
  game_points = 20;
  round_points = 5;
  ai_level = 0;
  collected_cards = lst2;
  p_num = 1
}

let player_state2 = {
  hand = lst3;
  game_points = 25;
  round_points = 5;
  ai_level = 0;
  collected_cards = lst4;
  p_num = 2
}

let player_state3 = {
  hand = lst3;
  game_points = 100;
  round_points = 100;
  ai_level = 0;
  collected_cards = lst4;
  p_num = 3
}

let player_state4 = {
  hand = lst3;
  game_points = 50;
  round_points = 0;
  ai_level = 0;
  collected_cards = lst4;
  p_num = 4
}

let game_state1 = {
  pool = pool1;
  prs = [player_state1; player_state2; player_state3; player_state4];
  phase = Play;
  round_num = 1;
  last_human_player = 3
}

let game_state2 = {
  pool = pool1;
  prs = [player_state3; player_state4; player_state1; player_state2];
  phase = Pass;
  round_num = 1;
  last_human_player = 1;
}

let window_width = 1280
let window_height = 750
let card_height = 90
let card_width = 60
let card_spacing = 5
let exit = ref true
let player_hand = ref []

let f_triple (a,_,_) = a
let s_triple (_,a,_) = a
let t_triple (_,_,a) = a

(* http://stackoverflow.com/questions/10068713/string-to-list-of-char *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let get_player_hand () =
  !player_hand

let clear_player_hand () = 
  player_hand := []

let init_window w h =
  let s = " " ^ (string_of_int w) ^ "x" ^ (string_of_int h) in
  let () = open_graph s in
  ()

let card_selection hand index =
  let c = List.nth hand index in
  let () = set_line_width 10 in 
  let () = set_color yellow in 
  draw_rect (s_triple c) (t_triple c) card_width card_height

let rec click_card () = 
  let s = wait_next_event [Key_pressed] in
  let tpl = (s.keypressed, s.key) in 
  let ph = get_player_hand () in 
  let card  = 
  (match tpl with 
  |(true, '`') -> card_selection ph 0; f_triple (List.nth ph 0)
  |(true, '1') -> card_selection ph 1; f_triple (List.nth ph 1) 
  |(true, '2') -> card_selection ph 2; f_triple (List.nth ph 2) 
  |(true, '3') -> card_selection ph 3; f_triple (List.nth ph 3) 
  |(true, '4') -> card_selection ph 4; f_triple (List.nth ph 4)
  |(true, '5') -> card_selection ph 5; f_triple (List.nth ph 5)
  |(true, '6') -> card_selection ph 6; f_triple (List.nth ph 6)
  |(true, '7') -> card_selection ph 7; f_triple (List.nth ph 7)
  |(true, '8') -> card_selection ph 8; f_triple (List.nth ph 8)
  |(true, '9') -> card_selection ph 9; f_triple (List.nth ph 9)
  |(true, '0') -> card_selection ph 10; f_triple (List.nth ph 10)
  |(true, '-') -> card_selection ph 11; f_triple (List.nth ph 11)
  |(true, '=') -> card_selection ph 12; f_triple (List.nth ph 12)) 
  in 
  card
(*   let s = wait_next_event [Key_pressed] in 
  if s.keypressed && s.key = '\r' then card else click_card () *)

let trade_cards () = 
  [click_card (); click_card(); click_card()]

let draw_symbol sym x y = 
  match sym with
  |Heart -> 
    set_color red;
    fill_poly [|(x,y);(x-10,y+10);(x-10,y+15);(x-5,y+15);(x,y+10);(x+5,y+15);(x+10,y+15);(x+10,y+10);(x,y)|]
  |Diamond -> 
    set_color red;
    fill_poly [|(x,y);(x+5,y+10);(x,y+20);(x-5,y+10);(x,y)|]
  |Spade ->
    set_color black;
    fill_poly [|(x,y);(x-5,y-5);(x-10,y);(x-10,y+5);(x,y+15);(x+10,y+5);(x+10,y);(x+5,y-5);(x,y)|];
    fill_rect (x-3) (y-10) 5 10
  |Club -> 
    set_color black;
    fill_circle (x-5) y 7;
    fill_circle (x+5) y 7;
    fill_circle x (y+10) 7;
    fill_rect (x-2) (y-10) 4 10

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
            set_color red;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+50) (y+10);
            draw_string card_char;
            moveto (x+ (int_of_float ((float card_width)/.6.0))) (y+40);
            draw_symbol Heart (x+(card_width/2)) (y+(card_height/2))
          end
        else if suit = Diamond then
          begin
            set_color red;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+50) (y+10);
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
  moveto ((x-(int_of_float (0.05*.(float window_width)))) - 30) (y+(int_of_float (0.1*.(float window_height))));
  draw_string (f_triple s);
  moveto ((x-(int_of_float (0.05*.(float window_width)))) - 30) ((y+(int_of_float (0.1*.(float window_height)))) - 20);
  draw_string (s_triple s);
  moveto ((x-(int_of_float (0.05*.(float window_width)))) - 30) ((y+(int_of_float (0.1*.(float window_height)))) - 40);
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

let draw_pool pool =
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
    draw_string ("Player " ^ (string_of_int (snd (List.nth pool i))));
    delta := !delta + (card_width + card_spacing);
  done;
  in ()

(* let draw_big_string lst x y =
  for i = 0 to (List.length lst) - 1 do
    
 *)


let draw_play_phase x y = 

  set_line_width 10;
   
  moveto x y;
  lineto x (y+20);
  lineto (x-10) (y+40);
  moveto x (y+20);
  lineto (x+10) (y+40);

  draw_ellipse (x+30) (y+20) 12 20;

  draw_arc (x+65) (y+20) 12 20 180 360;
  moveto (x+53) (y+20);
  lineto (x+53) (y+40);
  moveto (x+77) (y+20);
  lineto (x+77) (y+40);

  moveto (x+90) y;
  lineto (x+90) (y+40);
  draw_arc (x+90) (y+30) 25 10 (-90) 90;
  moveto (x+90) (y+20);
  lineto (x+115) y;

  moveto (x+170) y;
  lineto (x+170) (y+40);
  moveto (x+150) (y+40);
  lineto (x+190) (y+40);

  draw_arc (x+210) (y+20) 12 20 180 360;
  moveto (x+198) (y+20);
  lineto (x+198) (y+40);
  moveto (x+222) (y+20);
  lineto (x+222) (y+40);

  moveto (x+240) y;
  lineto (x+240) (y+40);
  draw_arc (x+240) (y+30) 25 10 (-90) 90;
  moveto (x+240) (y+20);
  lineto (x+265) y;

  moveto (x+280) y;
  lineto (x+280) (y+40);
  lineto (x+310) y;
  lineto (x+310) (y+40)

let draw_pass_phase x y =
  set_line_width 10;
  set_color black;

  draw_arc (x+15) (y+20) 15 20 45 305;

  moveto (x+40) y;
  lineto (x+40) (y+40);
  moveto (x+40) (y+20);
  lineto (x+60) (y+20);
  moveto (x+60) y;
  lineto (x+60) (y+40);

  draw_ellipse (x+85) (y+20) 12 20;
  draw_ellipse (x+120) (y+20) 12 20;

  draw_arc (x+155) (y+10) 10 10 (-150) 90;
  draw_arc (x+155) (y+30) 10 10 270 50;

  moveto (x+180) y;
  lineto (x+180) (y+40);
  lineto (x+210) (y+40);
  moveto (x+180) (y+20);
  lineto (x+200) (y+20);
  moveto (x+180) y;
  lineto (x+210) y;

  draw_arc (x+270) (y+30) 10 10 130 (-110);
  draw_arc (x+270) (y+10) 10 10 110 (-130)

let draw_phase phase x y = 
  match phase with
  |Play -> draw_play_phase x y
  |Pass -> draw_pass_phase x (y-300)
  |Setup -> draw_pass_phase x (y-300)

let rec switch_player () =
  clear_graph ();
  moveto (window_width/2) (window_height/2);
  draw_string "PRESS ENTER TO SWITCH PLAYER";
  let s = wait_next_event [Key_pressed] in 
  if s.keypressed && s.key = '\r' then () else switch_player ()

let rec winner pnum =
  clear_graph ();
  moveto (window_width/2) (window_height/2);
  draw_string "HAND GOES TO: ";
  moveto (window_width/2) ((window_height/2) - 20);
  draw_string ("Player " ^ (string_of_int pnum));
  moveto (window_width/2) ((window_height/2) - 40);
  moveto (window_width/2) (window_height - 40);
  draw_string "PRESS ENTER TO CONTINUE";
  let s = wait_next_event [Key_pressed] in 
  if s.keypressed && s.key = '\r' then () else winner pnum

let rec game_points lst = 
  clear_graph ();
  for i = 0 to (List.length lst) - 1 do
    moveto (window_width/2) ((window_height/2) - (i*20));
    draw_string ("Player " ^ (string_of_int i) ^ " has " ^ (string_of_int (List.nth lst i)) ^ " points")
  done;
  moveto (window_width/2) (window_height - 40);
  draw_string "PRESS ENTER TO CONTINUE";
  let s = wait_next_event [Key_pressed] in 
  if s.keypressed && s.key = '\r' then () else game_points lst

let rec find_index lst num acc =
  match lst with
  |[] -> acc
  |h::t -> if h.p_num == num then acc else find_index t num (acc+1)

let player_string state idx = 
  let player = "Player " ^ (string_of_int (List.nth state.prs idx).p_num) in  
  let game_points = "Game Points: " ^ (string_of_int (List.nth state.prs idx).game_points) in 
  let round_points = "Round Points: " ^ (string_of_int (List.nth state.prs idx).round_points) in
  (player, game_points, round_points)

let draw_board state pstate =
  init_window window_width window_height;
  set_window_title "CS 3110 Hearts Game";
  clear_graph ();
  let num = List.length (pstate.hand) in
  let index = find_index state.prs pstate.p_num 0 in
  let human_index = find_index state.prs state.last_human_player 0 in
  let human_pstate = List.nth state.prs human_index in  
  let left_index = (human_index + 1) mod 4 in 
  let top_index = (human_index + 2) mod 4 in 
  let right_index = (human_index + 3) mod 4 in 
  if (state.last_human_player = pstate.p_num) then 
    draw_phase state.phase (int_of_float (0.375*.(float window_width))) (int_of_float (0.65*.(float window_height)));
  draw_card_top num ((int_of_float (0.30*.(float window_width)))) (int_of_float (0.8*.(float window_height))) (player_string state top_index);
  draw_card_side num (int_of_float (0.095*.(float window_width))) ((int_of_float (0.20*.(float window_height)))) (player_string state left_index) true;
  draw_card_side num ((int_of_float (0.905*.(float window_width))) - card_height) ((int_of_float (0.20*.(float window_height)))) (player_string state right_index) false;
  draw_pool state.pool;
  draw_hand human_pstate.hand (player_string state human_index);
(*   switch_player (); *)
(*   game_points [1;2;3;4]; *)
(*   while true do (); done *)
(* 
let () = draw_board game_state1 player_state1 *)
