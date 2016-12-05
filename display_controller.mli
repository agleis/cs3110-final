open Types

(*
 * [draw_board gs ps] displays the game with the given game state and current player.
*)
val draw_board: game_state -> player_state -> unit

(*
 * [click_card cl gs ps] Takes in the game and player state and returns the card
 * chosen by the human player during the Play phase
*)
val click_card: card list -> game_state -> player_state  -> card

(*
 * [trade_cards cl gs ps] Takes in teh game and player state and returns the card list
 * that the human player chose during the Pass phase
*)
val trade_cards: card list -> game_state -> player_state -> card list

(*
 * [switch_player s] Switches screen of the gui to display
 * and waits for player [s] to press enter
*)
val switch_player: string -> unit

(*
 * [winner gs x] Displays a screen with all the
 * players and the points they gaind that round
*)
val winner: game_state -> int -> unit

(*
 * [game_points psl] Displays screen with all the
 * players and the points they gained that game
*)
val game_points: player_state list -> unit

(*
 * [draw_end_game psl] Displays screen with
 * player with lowest score as winner and the
 * rest of the players' game score
*)
val draw_end_game: player_state list -> unit

val init_window : int -> int -> unit
