open Graphics
#load "graphics.cma"

exception End;;

type suit = Heart | Club | Diamond | Spade

type state = Setup | Pass | Play

type card = {
  suit: suit;
  value: int;
}

(* let lst1 = [(1,"Hearts");(2,"Hearts");(3,"Clubs");(4,"Clubs");(5,"Diamonds");(6,"Diamonds");(7,"Spades");(8,"Spades");(9,"Hearts");(10,"Clubs");(11,"Diamonds");(12,"Spades");(13,"Hearts")]
let lst2 = [(1,"Hearts");(2,"Hearts");(3,"Clubs");(4,"Clubs");(5,"Diamonds");(6,"Diamonds");(7,"Spades")] *)
let lst1 = [{suit=Heart; value=1};{suit=Heart; value=2};{suit=Club; value=3};{suit=Club; value=4};{suit=Diamond; value=5};{suit=Diamond; value=6}; {suit=Spade; value=7}; {suit=Spade; value=8}; {suit=Heart; value=9}; {suit=Club; value=10};{suit=Diamond; value=11};{suit=Spade; value=12};{suit=Heart; value=13}]
let lst2 = [{suit=Heart; value=1};{suit=Heart; value=2};{suit=Club; value=3};{suit=Club; value=4};{suit=Diamond; value=5};{suit=Diamond; value=6}]
let pool1 = [{suit=Diamond; value=5}; {suit=Diamond; value=6}; {suit=Spade; value=7}; {suit=Spade; value=8}]
(* let pool1 = [(5,"Diamonds");(6,"Diamonds");(7,"Spades");(8,"Spades")] *)

let window_width = 1280
let window_height = 750
let card_spacing = 5
let player_hand = ref []

(* let diamond = Graphics.fill_poly [|(10,10);(15,20);(10,30);(5,20);(10,10)|]
let club = 
  Graphics.fill_circle 100 100 7;;
  Graphics.fill_circle 110 100 7;;
  Graphics.fill_circle 105 110 7;;
  Graphics.fill_rect 103 90 4 10;;
let heart = Graphics.fill_poly [|(10,10);(0,20);(0,25);(5,25);(10,20);(15,25);(20,25);(20,20);(10,10)|];;
let spade = 
  Graphics.draw_poly [|(100,100);(95,100);(95,105);(100,110);(105,105);(105,100)|];;
  Graphics.draw_rect 98 95 4 5;; *)

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

let draw_symbol sym x y = 
  match sym with
  |Heart -> 
    Graphics.set_color Graphics.red;
    Graphics.fill_poly [|(x,y);(x-10,y+10);(x-10,y+15);(x-5,y+15);(x,y+10);(x+5,y+15);(x+10,y+15);(x+10,y+10);(x,y)|]
  |Diamond -> 
    Graphics.set_color Graphics.red;
    Graphics.fill_poly [|(x,y);(x+5,y+10);(x,y+20);(x-5,y+10);(x,y)|]
  |Spade ->
    Graphics.set_color Graphics.black;
    Graphics.fill_poly [|(x,y);(x-5,y-5);(x-10,y);(x-10,y+5);(x,y+15);(x+10,y+5);(x+10,y);(x+5,y-5);(x,y)|];
    Graphics.fill_rect (x-3) (y-10) 5 10
  |Club -> 
    Graphics.set_color Graphics.black;
    Graphics.fill_circle (x-5) y 7;
    Graphics.fill_circle (x+5) y 7;
    Graphics.fill_circle x (y+10) 7;
    Graphics.fill_rect (x-2) (y-10) 4 10

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
        if suit = Heart then
          begin
            Graphics.set_color Graphics.red;
            Graphics.moveto (x+5) (y+75);
            Graphics.draw_string card_char;
            Graphics.moveto (x+50) (y+10);
            Graphics.draw_string card_char;
            Graphics.moveto (x+ (int_of_float ((float card_width)/.6.0))) (y+40);
            draw_symbol Heart (x+(cw/2)) (y+(ch/2))
          end
        else if suit = Diamond then
          begin
            Graphics.set_color Graphics.red;
            Graphics.moveto (x+5) (y+75);
            Graphics.draw_string card_char;
            Graphics.moveto (x+50) (y+10);
            Graphics.draw_string card_char;
            Graphics.moveto (x+ (int_of_float ((float card_width)/.6.0))) (y+40);
            draw_symbol Diamond (x+(cw/2)) (y+(ch/2))
          end
        else if suit = Spade then
          begin
            let z = if num == 10 then 45 else 50 in 
            Graphics.set_color Graphics.black;
            Graphics.moveto (x+5) (y+75);
            Graphics.draw_string card_char;
            Graphics.moveto (x+z) (y+10);
            Graphics.draw_string card_char;
            Graphics.moveto (x+10) (y+40);
            draw_symbol Spade (x+(cw/2)) (y+(ch/2))
          end
        else 
          begin
            let z = if num == 10 then 45 else 50 in 
            Graphics.set_color Graphics.black;
            Graphics.moveto (x+5) (y+75);
            Graphics.draw_string card_char;
            Graphics.moveto (x+z) (y+10);
            Graphics.draw_string card_char;
            Graphics.moveto (x+10) (y+40);
            draw_symbol Club (x+(cw/2)) (y+(ch/2))
          end
      end
    in 
    ()

let draw_hand lst w h cw ch=
  let delta = ref 0 in
  let len = List.length lst in 
  let total_len = (len * (cw + card_spacing)) in 
  let () = print_int total_len in 
  let () = print_string "\n" in 
  let () = print_int cw in 
  let () = print_string "\n" in 
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
    draw_card ((List.nth pool i).value) ((List.nth pool i).suit) xpos (int_of_float (0.45*.(float h))) cw ch;
    delta := !delta + (cw + card_spacing);
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
(*   init_window window_width window_height; *)
  Graphics.clear_graph (); 
  let num = 13 in 
  let player = 1 in 
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in 
  let () = print_int width in 
  let () = print_string "\n" in
  let () = print_int height in 
  let () = print_string "\n" in
  let card_width = int_of_float ((float width)*.0.046875) in 
  let card_height = int_of_float ((float height)*.0.12) in 
  draw_card_top num ((int_of_float (0.30*.(float width)))) (int_of_float (0.8*.(float height))) width height card_width card_height;
  draw_card_side num (int_of_float (0.05*.(float width))) ((int_of_float (0.20*.(float height)))) width height card_width card_height;
  draw_card_side num ((int_of_float (0.95*.(float width))) - card_height) ((int_of_float (0.20*.(float height)))) width height card_width card_height;
  draw_hand lst width height card_width card_height;
  draw_pool pool width height card_width card_height;
  draw_player player width height