open Graphics
open Types
(* #load "cma" *)

exception End;;

let lst1 = [{suit=Heart; value=1};{suit=Heart; value=2};{suit=Club; value=3};{suit=Club; value=4};{suit=Diamond; value=5};{suit=Diamond; value=6}; {suit=Spade; value=7}; {suit=Spade; value=8}; {suit=Heart; value=9}; {suit=Club; value=10};{suit=Diamond; value=11};{suit=Spade; value=12};{suit=Heart; value=13}]
let lst2 = [{suit=Heart; value=1};{suit=Heart; value=2};{suit=Club; value=3};{suit=Club; value=4};{suit=Diamond; value=5};{suit=Diamond; value=6}]
let lst3 = [{suit=Spade; value=1};{suit=Club; value=2};{suit=Club; value=3};{suit=Diamond; value=4};{suit=Diamond; value=5};{suit=Heart; value=6}; {suit=Spade; value=7}; {suit=Club; value=8}; {suit=Diamond; value=9}; {suit=Club; value=10};{suit=Heart; value=11};{suit=Spade; value=12};{suit=Diamond; value=13}]
let lst4 = [{suit=Heart; value=7};{suit=Heart; value=8};{suit=Club; value=9};{suit=Club; value=10};{suit=Diamond; value=11};{suit=Diamond; value=12}]
let pool1 = [({suit=Diamond; value=5},1); ({suit=Diamond; value=6},2); ({suit=Spade; value=7},3); ({suit=Spade; value=8},4)]

let player_state1 = {
  hand = lst1;
  round_points = 2;
  game_points = 20;
  ai_level = 0;
  collected_cards = lst2;
  p_num = 1
}

let player_state2 = {
  hand = lst3;
  round_points = 5;
  game_points = 25;
  ai_level = 0;
  collected_cards = lst4;
  p_num = 2
}

let game_state1 = {
  pool = pool1;
  prs = [player_state1; player_state2];
  phase = Play;
  round_num = 1
}

let window_width = 1280
let window_height = 750
let card_spacing = 5
let player_hand = ref []
let exit = ref true

let init_window w h =
  let s = " " ^ (string_of_int w) ^ "x" ^ (string_of_int h) in
  let () = open_graph s in
  ()

let click_card w h =
    let s = wait_next_event [Button_down; Button_up] in
    if s.button && s.mouse_x < (int_of_float (0.075*.(float w))) && s.mouse_y > (h-(int_of_float (0.025*.(float w)))) then
      close_graph (); exit := false

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

let draw_card num suit x y cw ch = 
    let card_char = 
    if num = 11 then "J" 
    else if num = 12 then "Q"
    else if num = 13 then "K"
    else if num = 14 || num = 1 then "A"
    else if num = 0 then "V"
    else if num = -1 then "H"
    else string_of_int num in 
    let card_width = cw in 
    let card_height = ch in 
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
            draw_symbol Heart (x+(cw/2)) (y+(ch/2))
          end
        else if suit = Diamond then
          begin
            set_color red;
            moveto (x+5) (y+75);
            draw_string card_char;
            moveto (x+50) (y+10);
            draw_string card_char;
            moveto (x+ (int_of_float ((float card_width)/.6.0))) (y+40);
            draw_symbol Diamond (x+(cw/2)) (y+(ch/2))
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
            draw_symbol Spade (x+(cw/2)) (y+(ch/2))
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
            draw_symbol Club (x+(cw/2)) (y+(ch/2))
          end
      end
    in 
    ()

let draw_hand lst w h cw ch=
  let delta = ref 0 in
  let len = List.length lst in
  let total_len = (len * (cw + card_spacing)) in
  let () =
  for i=0 to (List.length lst) - 1 do
      let xpos = ((int_of_float (0.5*.(float w))) + !delta - (total_len/2)) in
      let ypos = (int_of_float (0.06*.(float h))) in
      draw_card ((List.nth lst i).value) ((List.nth lst i).suit) xpos ypos cw ch;
      player_hand := !player_hand@[(List.nth lst i,xpos,ypos)];
      delta := !delta + (cw + card_spacing);
    done;
  in ()

let draw_card_top num x y w h cw ch =
  let () =
  let delta = ref 0 in
  for i=1 to num do
    draw_card 0 Spade (x + !delta) y cw ch;
    delta := !delta + (int_of_float ((float w)*.0.03))
  done;
  in ()

let draw_card_side num x y w h cw ch =
  let () =
  let delta = ref 0 in
  for i=1 to num do
    draw_card (-1) Spade x (y + !delta) cw ch;
    delta := !delta + (int_of_float ((float h)*.0.05))
  done;
  in ()

let draw_pool pool w h cw ch =
  let delta = ref 0 in
  let len = List.length pool in
  let total_len = (len * (cw + card_spacing)) in
  let () =
  for i=0 to (List.length pool) - 1 do
    let xpos = ((int_of_float (0.5*.(float w))) + !delta - (total_len/2)) in
    draw_card ((fst (List.nth pool i)).value) ((fst (List.nth pool i)).suit) xpos (int_of_float (0.45*.(float h))) cw ch;
    delta := !delta + (cw + card_spacing);
  done;
  in ()

let draw_player player w h =
  let () =
    set_color black;
    moveto ((int_of_float (0.05*.(float w)))) (int_of_float (0.95*.(float h)));
    draw_string ("Player " ^ (string_of_int player) ^ "'s turn");
    moveto ((int_of_float (0.05*.(float w)))) (int_of_float (0.90*.(float h)));
    draw_string ("Points: " ^ (string_of_int 0))
  in ()

let draw_quit w h =
  set_color green;
  fill_rect 0 (h-(int_of_float (0.025*.(float w)))) (int_of_float (0.075*.(float w))) (int_of_float (0.025*.(float w)));
  set_color black;
  moveto ((int_of_float (0.35*.0.075*.(float w)))) (h-(int_of_float (0.6*.0.025*.(float w))));
  draw_string "QUIT"

let draw_board state current_player_state =
  init_window window_width window_height;
  set_window_title "CS 3110 Hearts Game";
  clear_graph ();
  let num = List.length (current_player_state.hand) in
  let player = current_player_state.p_num in
  let lst = current_player_state.hand in 
  let pool = state.pool in 
  let width = size_x () in
  let height = size_y () in
  let card_width = int_of_float ((float width)*.0.046875) in
  let card_height = int_of_float ((float height)*.0.12) in
  draw_quit width height;
  draw_card_top num ((int_of_float (0.30*.(float width)))) (int_of_float (0.8*.(float height))) width height card_width card_height;
  draw_card_side num (int_of_float (0.05*.(float width))) ((int_of_float (0.20*.(float height)))) width height card_width card_height;
  draw_card_side num ((int_of_float (0.95*.(float width))) - card_height) ((int_of_float (0.20*.(float height)))) width height card_width card_height;
  draw_hand lst width height card_width card_height;
  draw_pool pool width height card_width card_height;
  draw_player player width height;
  click_card width height;
  while !exit do (); done

let () = draw_board game_state1 player_state1
