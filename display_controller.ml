open Graphics

let lst1 = [1;2;3;4;5;6;7;8;9;10;11;12;13]
let pool1 = [5;6;7;8]

let init_window w h =
  let s = " " ^ (string_of_int w) ^ "x" ^ (string_of_int h) in
  let () = Graphics.open_graph s in
  ()

let draw_card num x y =
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
        Graphics.fill_rect x y 60 90;
        Graphics.set_color Graphics.white;
        Graphics.fill_rect (x+2) (y+2) 56 86;
        Graphics.set_color Graphics.red;
        Graphics.fill_rect (x+5) (y+5) 50 80
      end
    else if card_char = "H" then
      begin
        Graphics.set_color Graphics.black;
        Graphics.fill_rect x y 90 60;
        Graphics.set_color Graphics.white;
        Graphics.fill_rect (x+2) (y+2) 86 56;
        Graphics.set_color Graphics.red;
        Graphics.fill_rect (x+5) (y+5) 80 50
      end
    else
      begin
        Graphics.set_color Graphics.black;
        Graphics.fill_rect x y 60 90;
        Graphics.set_color Graphics.white;
        Graphics.fill_rect (x+2) (y+2) 56 86;
        Graphics.set_color Graphics.red;
        Graphics.moveto (x+30) (y+40);
        Graphics.draw_string card_char
      end
    in
    ()

let draw_hand lst w h=
  let y = ref 0 in
  let () =
  for i=1 to (List.length lst) - 1 do
      draw_card (List.nth lst i) ((int_of_float (0.15*.(float w))) + !y) (int_of_float (0.06*.(float h)));
      y := !y + (int_of_float ((float w)*.0.065));
    done;
  in ()

let draw_card_top num x y w h =
  let () =
  let delta = ref 0 in
  for i=1 to num do
    draw_card 0 (x + !delta) y;
    delta := !delta + (int_of_float ((float w)*.0.03))
  done;
  in ()

let draw_card_side num x y w h =
  let () =
  let delta = ref 0 in
  for i=1 to num do
    draw_card (-1) x (y + !delta);
    delta := !delta + (int_of_float ((float h)*.0.05))
  done;
  in ()

let draw_pool pool w h=
  let delta = ref 0 in
  let () =
  for i=0 to (List.length pool) - 1 do
    draw_card (List.nth pool i) ((int_of_float (0.4*.(float w))) + !delta) (int_of_float (0.45*.(float h)));
    delta := !delta + 65;
  done;
  in ()

let draw_player player w h=
  let () =
    Graphics.moveto ((int_of_float (0.05*.(float w)))) (int_of_float (0.95*.(float h)));
    Graphics.draw_string ("Player " ^(string_of_int player) ^ " turn")
  in ()

let draw_board state lst pool =
  let num = 13 in
  let player = 1 in
  let width = Graphics.size_x () in
  let height = Graphics.size_y () in
  draw_card_top num ((int_of_float (0.30*.(float width)))) (int_of_float (0.8*.(float height))) width height;
  draw_card_side num (int_of_float (0.1*.(float width))) ((int_of_float (0.20*.(float height)))) width height;
  draw_card_side num (int_of_float (0.875*.(float width))) ((int_of_float (0.20*.(float height)))) width height;
  draw_hand lst width height;
  draw_pool pool width height;
  draw_player player width height
