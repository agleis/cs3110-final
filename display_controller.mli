open Types

(* [display_state game_state] displays the game with the given game state. *)
val draw_board: Types.game_state -> Types.player_state -> unit

val click_card: card list -> Types.game_state -> Types.player_state  -> card

val trade_cards: card list -> Types.game_state -> Types.player_state -> card list

val switch_player: unit -> unit

val winner: Types.game_state -> int -> unit

val game_points: int list -> unit

val draw_end_game: int list -> unit

val draw_string1 : string -> int -> int -> unit
