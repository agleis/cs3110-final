open Types
open Human_controller
open Ai_controller

(* [main p_lst name_lst] is the main entry point of the program - kicks off
* REPL loop*)
val main : int list -> string list -> unit

(* [initialize_state a n d] is a record of type game_state that is
 * takes in a list of ai_levels, a list of names, and a deck of cards
 * and initializes to the correct fields and deals the deck to the players*)
val initialize_state : int list -> string list -> card list -> game_state
