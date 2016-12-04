open Types

(* [display_state game_state] displays the game with the given game state. *)
val draw_board: Types.game_state -> Types.player_state -> unit

val click_card: unit -> card

val trade_cards: unit -> card list

val switch_player: unit -> unit

val winner: int -> unit 

val game_points: int list -> unit

