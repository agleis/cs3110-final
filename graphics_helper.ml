open Graphics
open Types

(* http://stackoverflow.com/questions/10068713/string-to-list-of-char *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let draw_A x y =
  moveto x y;
  lineto (x+15) (y+40);
  lineto (x+30) y;
  moveto (x+7) (y+15);
  lineto (x+25) (y+15)

let draw_B x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+30) (y+30);
  lineto x (y+20);
  lineto (x+30) (y+10);
  lineto x y

let draw_C x y =
  moveto (x+30) (y+40);
  lineto x (y+40);
  lineto x y;
  lineto (x+30) y

let draw_D x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+30) (y+20);
  lineto x y

let draw_E x y =
  moveto (x+30) (y+40);
  lineto x (y+40);
  lineto x y;
  lineto (x+30) y;
  moveto x (y+20);
  lineto (x+25) (y+20)

let draw_F x y =
  moveto (x+30) (y+40);
  lineto x (y+40);
  lineto x y;
  moveto x (y+20);
  lineto (x+25) (y+20)

let draw_G x y =
  moveto (x+30) (y+40);
  lineto x (y+40);
  lineto x y;
  lineto (x+30) y;
  lineto (x+30) (y+20);
  lineto (x+20) (y+20)

let draw_H x y =
  moveto x y;
  lineto x (y+40);
  moveto (x+30) y;
  lineto (x+30) (y+40);
  moveto x (y+20);
  lineto (x+30) (y+20)

let draw_I x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  moveto x y;
  lineto (x+30) y;
  moveto (x+15) y;
  lineto (x+15) (y+40)

let draw_J x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  moveto (x+15) (y+40);
  lineto (x+15) y;
  lineto x y;
  lineto x (y+10)

let draw_K x y =
  moveto x y;
  lineto x (y+40);
  moveto (x+30) (y+40);
  lineto x (y+20);
  lineto (x+30) y

let draw_L x y =
  moveto x (y+40);
  lineto x y;
  lineto (x+30) y

let draw_M x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+15) (y+20);
  lineto (x+30) (y+40);
  lineto (x+30) y

let draw_N x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+30) y;
  lineto (x+30) (y+40)

let draw_O x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+30) (y+40);
  lineto (x+30) y;
  lineto x y

let draw_P x y =
  moveto x (y+20);
  lineto (x+30) (y+20);
  lineto (x+30) (y+40);
  lineto x (y+40);
  lineto x y

let draw_Q x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+25) (y+40);
  lineto (x+25) y;
  lineto x y;
  moveto (x+30) y;
  lineto (x+25) y;
  lineto (x+20) (y+5)

let draw_R x y =
  moveto x (y+20);
  lineto (x+30) (y+20);
  lineto (x+30) (y+40);
  lineto x (y+40);
  lineto x y;
  moveto x (y+20);
  lineto (x+30) y

let draw_S x y =
  moveto (x+30) (y+40);
  lineto x (y+40);
  lineto x (y+20);
  lineto (x+30) (y+20);
  lineto (x+30) y;
  lineto x y

let draw_T x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  moveto (x+15) (y+40);
  lineto (x+15) y

let draw_U x y =
  moveto x (y+40);
  lineto x y;
  lineto (x+30) y;
  lineto (x+30) (y+40)

let draw_V x y =
  moveto x (y+40);
  lineto (x+15) y;
  lineto (x+30) (y+40)

let draw_W x y =
  moveto x (y+40);
  lineto x y;
  lineto (x+15) y;
  lineto (x+15) (y+20);
  lineto (x+15) y;
  lineto (x+30) y;
  lineto (x+30) (y+40)

let draw_X x y =
  moveto x (y+40);
  lineto (x+30) y;
  moveto (x+30) (y+40);
  lineto x y

let draw_Y x y =
  moveto x (y+40);
  lineto (x+15) (y+20);
  lineto (x+30) (y+40);
  moveto (x+15) (y+20);
  lineto (x+15) y

let draw_Z x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  lineto x y;
  lineto (x+30) y

let draw_1 x y =
  moveto (x+15) y;
  lineto (x+15) (y+40);
  lineto (x+10) (y+40);
  moveto (x+15) y;
  lineto (x+10) y;
  moveto x y;
  lineto (x+30) y

let draw_2 x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  lineto (x+30) (y+20);
  lineto x (y+20);
  lineto x y;
  lineto (x+30) y

let draw_3 x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  lineto (x+30) (y+20);
  lineto x (y+20);
  moveto (x+30) (y+20);
  lineto (x+30) y;
  lineto x y

let draw_4 x y =
  moveto x (y+40);
  lineto x (y+20);
  lineto (x+30) (y+20);
  moveto (x+30) (y+40);
  lineto (x+30) y

let draw_6 x y =
  moveto (x+30) (y+40);
  lineto x (y+40);
  lineto x y;
  lineto (x+30) y;
  lineto (x+30) (y+20);
  lineto x (y+20)

let draw_7 x y =
  moveto x (y+40);
  lineto (x+30) (y+40);
  lineto (x+30) y

let draw_8 x y =
  moveto x y;
  lineto x (y+40);
  lineto (x+30) (y+40);
  lineto (x+30) y;
  lineto x y;
  moveto x (y+20);
  lineto (x+30) (y+20)

let draw_9 x y =
  moveto (x+30) y;
  lineto (x+30) (y+40);
  lineto x (y+40);
  lineto x (y+20);
  lineto (x+30) (y+20)

let draw_Period x y =
  moveto x y;
  lineto x (y+5);
  lineto (x+5) (y+5);
  lineto (x+5) (y);
  lineto x y

let draw_Colon x y =
  moveto x (y+5);
  lineto x (y+10);
  lineto (x+5) (y+10);
  lineto (x+5) (y+5);
  lineto x (y+5);
  moveto (x) (y+20);
  lineto x (y+25);
  lineto (x+5) (y+25);
  lineto (x+5) (y+20);
  lineto (x) (y+20)

let draw_letter ch x y =
  match ch with
  |'A' -> draw_A x y
  |'B' -> draw_B x y
  |'C' -> draw_C x y
  |'D' -> draw_D x y
  |'E' -> draw_E x y
  |'F' -> draw_F x y
  |'G' -> draw_G x y
  |'H' -> draw_H x y
  |'I' -> draw_I x y
  |'J' -> draw_J x y
  |'K' -> draw_K x y
  |'L' -> draw_L x y
  |'M' -> draw_M x y
  |'N' -> draw_N x y
  |'O' -> draw_O x y
  |'P' -> draw_P x y
  |'Q' -> draw_Q x y
  |'R' -> draw_R x y
  |'S' -> draw_S x y
  |'T' -> draw_T x y
  |'U' -> draw_U x y
  |'V' -> draw_V x y
  |'W' -> draw_W x y
  |'X' -> draw_X x y
  |'Y' -> draw_Y x y
  |'Z' -> draw_Z x y
  |'0' -> draw_O x y
  |'1' -> draw_1 x y
  |'2' -> draw_2 x y
  |'3' -> draw_3 x y
  |'4' -> draw_4 x y
  |'5' -> draw_S x y
  |'6' -> draw_6 x y
  |'7' -> draw_7 x y
  |'8' -> draw_8 x y
  |'9' -> draw_9 x y
  |'.' -> draw_Period x y
  |':' -> draw_Colon x y
  |_ -> failwith "Not a letter"

let draw_left_arrow x y =
    set_color black;
    moveto x y;
    lineto (x - 400) y;
    lineto (x - 350) (y - 50);
    moveto (x - 400) y;
    lineto (x - 350) (y + 50)

    (* code for arrow left*)

let draw_right_arrow x y =
    set_color black;
    moveto x y;
    lineto (x + 400) y;
    lineto (x + 350) (y - 50);
    moveto (x + 400) y;
    lineto (x + 350) (y + 50)
(* code for arrow left*)

let draw_across_arrow x y =
    set_color black;
    moveto x y;
    lineto x (y+300);
    lineto (x - 50) (y + 250);
    moveto x (y+300);
    lineto (x + 50) (y + 250)
(* code for arrow left*)

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

