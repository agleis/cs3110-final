open Graphics
#load "graphics.cma"

exception End;;

let lst1 = [(1,"Hearts");(2,"Hearts");(3,"Clubs");(4,"Clubs");(5,"Diamonds");(6,"Diamonds");(7,"Spades");(8,"Spades");(9,"Hearts");(10,"Clubs");(11,"Diamonds");(12,"Spades");(13,"Hearts")]
let lst2 = [(1,"Hearts");(2,"Hearts");(3,"Clubs");(4,"Clubs");(5,"Diamonds");(6,"Diamonds");(7,"Spades")]
let pool1 = [(5,"Diamonds");(6,"Diamonds");(7,"Spades");(8,"Spades")]
let card_width = 60
let card_height = 90
let window_width = 1000
let window_height = 750
let player_hand = ref []

let init_window w h =
  let s = " " ^ (string_of_int w) ^ "x" ^ (string_of_int h) in 
  let () = Graphics.open_graph s in
  ()

let skel f_init f_end f_key f_mouse f_except = 
  f_init ();
  try 
      while true do 
        try 
          let s = Graphics.wait_next_event 
                    [Graphics.Button_down; Graphics.Key_pressed] 
          in if s.Graphics.keypressed then f_key s.Graphics.key
             else if s.Graphics.button 
                  then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
        with 
             End -> raise End
           |  e  -> f_except e
      done
  with 
      End  -> f_end ();;

let click_card lst = 
    let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Button_up] in 
    if s.Graphics.button then Graphics.close_graph ()

let draw_card num suit x y = 
    let card_char = 
    if num = 11 then "J" 
    else if num = 12 then "Q"
    else if num = 13 then "K"
    else if num = 14 || num = 1 then "A"
    else if num = 0 then "V"
    else if num = -1 then "H"
    else string_of_int num in 
    let (w,h) = Graphics.text_size "h" in 
    let () = 
    if card_char = "V" then 
      begin 
        Graphics.set_color Graphics.black;
        Graphics.fill_rect x y card_width card_height;
        Graphics.set_color Graphics.white;
        Graphics.fill_rect (x+2) (y+2) (card_width - 4) (card_height - 4);
        Graphics.set_color Graphics.red;
        Graphics.fill_rect (x+5) (y+5) (card_width - 10) (card_height - 10)
      end
    else if card_char = "H" then
      begin
        Graphics.set_color Graphics.black;
        Graphics.fill_rect x y card_height card_width;
        Graphics.set_color Graphics.white;
        Graphics.fill_rect (x+2) (y+2) (card_height - 4) (card_width - 4);
        Graphics.set_color Graphics.red;
        Graphics.fill_rect (x+5) (y+5) (card_height - 10) (card_width - 10)
      end
    else 
      begin
        Graphics.set_color Graphics.black;
        Graphics.fill_rect x y card_width card_height;
        Graphics.set_color Graphics.white;
        Graphics.fill_rect (x+2) (y+2) (card_width - 4) (card_height - 4);
        if suit = "Hearts" || suit = "Diamonds" then
          begin
            Graphics.set_color Graphics.red;
            Graphics.moveto (x+5) (y+75);
            Graphics.draw_string card_char;
            Graphics.moveto (x+50) (y+10);
            Graphics.draw_string card_char;
            Graphics.moveto (x+10) (y+40);
            Graphics.draw_string suit
          end
        else
          begin
            Graphics.set_color Graphics.black;
            Graphics.moveto (x+5) (y+75);
            Graphics.draw_string card_char;
            Graphics.moveto (x+50) (y+10);
            Graphics.draw_string card_char;
            Graphics.moveto (x+10) (y+40);
            Graphics.draw_string suit
          end
      end
    in 
    ()

let draw_hand lst w h=
  let delta = ref 0 in
  let len = List.length lst in 
  let total_len = (len * card_width) in 
  let () = 
  for i=0 to (List.length lst) - 1 do
      let xpos = ((int_of_float (0.5*.(float w))) + !delta - (total_len/2)) in  
      let ypos = (int_of_float (0.06*.(float h))) in 
      draw_card (fst (List.nth lst i)) (snd (List.nth lst i)) xpos ypos;
      player_hand := !player_hand@[(List.nth lst i,xpos,ypos)];
      delta := !delta + (int_of_float ((float w)*.0.065));
    done;
  in ()

let draw_card_top num x y w h = 
  let () = 
  let delta = ref 0 in 
  for i=1 to num do
    draw_card 0 "" (x + !delta) y;
    delta := !delta + (int_of_float ((float w)*.0.03))
  done;
  in ()

let draw_card_side num x y w h = 
  let () = 
  let delta = ref 0 in 
  for i=1 to num do
    draw_card (-1) "" x (y + !delta);
    delta := !delta + (int_of_float ((float h)*.0.05))
  done;
  in ()

let draw_pool pool w h= 
  let delta = ref 0 in 
  let () = 
  for i=0 to (List.length pool) - 1 do
    draw_card (fst (List.nth pool i)) (snd (List.nth pool i)) ((int_of_float (0.4*.(float w))) + !delta) (int_of_float (0.45*.(float h)));
    delta := !delta + 65;
  done;
  in ()

let draw_player player w h= 
  let () = 
    Graphics.set_color Graphics.black;
    Graphics.moveto ((int_of_float (0.05*.(float w)))) (int_of_float (0.95*.(float h)));
    Graphics.draw_string ("Player " ^ (string_of_int player) ^ "'s turn");
    Graphics.moveto ((int_of_float (0.05*.(float w)))) (int_of_float (0.90*.(float h)));
    Graphics.draw_string ("Points: " ^ (string_of_int 0))
  in ()

let draw_board state lst pool = 
  init_window window_width window_height;
  let num = 13 in 
  let player = 1 in 
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in 
  draw_card_top num ((int_of_float (0.30*.(float width)))) (int_of_float (0.8*.(float height))) width height;
  draw_card_side num (int_of_float (0.1*.(float width))) ((int_of_float (0.20*.(float height)))) width height;
  draw_card_side num (int_of_float (0.865*.(float width))) ((int_of_float (0.20*.(float height)))) width height;
  draw_hand lst width height;
  draw_pool pool width height;
  draw_player player width height