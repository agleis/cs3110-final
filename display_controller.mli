open Types

(* [display_state game_state] displays the game with the given game state. *)
val draw_board: game_state -> player_state -> unit

val click_card: card list -> game_state -> player_state  -> card

val trade_cards: card list -> game_state -> player_state -> card list

val switch_player: unit -> unit

val winner: game_state -> int -> unit

val game_points: player_state list -> unit

val draw_end_game: player_state list -> unit

val draw_string1 : string -> int -> int -> unit
