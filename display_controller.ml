
let draw_board x = 
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in 
  let y = ref 0 in 
  let z = ref 0 in 
  for i=1 to x do
    draw_card_top ((int_of_float (0.30*.(float width))) + !y) (int_of_float (0.8*.(float height)));
    draw_card_side (int_of_float (0.1*.(float width))) ((int_of_float (0.20*.(float height))) + !z);
    draw_card_side (int_of_float (0.875*.(float width))) ((int_of_float (0.20*.(float height))) + !z);
    y := !y + (int_of_float ((float width)*.0.025));
    z := !z + (int_of_float ((float height)*.0.05))
  done;

let draw_hand x =
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in 
  let y = ref 0 in
  for i=1 to x do
      let num = (Random.int 14) + 1 in 
      draw_card num ((int_of_float (0.15*.(float width))) + !y) (int_of_float (0.08*.(float height)));
      y := !y + (int_of_float ((float width)*.0.065));
    done;

let draw_card num x y = 
    let card_char = 
    if num = 11 then "J" 
    else if num = 12 then "Q"
    else if num = 13 then "K"
    else if num = 14 || num = 1 then "A"
    else string_of_int num in 
    let (w,h) = Graphics.text_size "h" in 
    Graphics.set_color Graphics.black;
    Graphics.fill_rect x y 40 70;
    Graphics.set_color Graphics.white;
    Graphics.fill_rect (x+2) (y+2) 36 66;
    Graphics.set_color Graphics.red;
    Graphics.moveto (x+20-h) (y+35-h);
    Graphics.draw_string card_char

let draw_card_top x y = 
    Graphics.set_color Graphics.black;
    Graphics.fill_rect x y 40 70;
    Graphics.set_color Graphics.white;
    Graphics.fill_rect (x+2) (y+2) 36 66;
    Graphics.set_color Graphics.red;
    Graphics.fill_rect (x+5) (y+5) 30 60;

let draw_card_side x y = 
    Graphics.set_color Graphics.black;
    Graphics.fill_rect x y 70 40;
    Graphics.set_color Graphics.white;
    Graphics.fill_rect (x+2) (y+2) 66 36;
    Graphics.set_color Graphics.red;
    Graphics.fill_rect (x+5) (y+5) 60 30;












