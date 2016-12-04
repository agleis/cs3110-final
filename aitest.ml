open OUnit2
open Types
open Randoms
open Ai_controller
open Ai_helpers

let akq_hand = [
  {suit = Spade; value = 14};
  {suit = Spade; value = 13};
  {suit = Spade; value = 12};
  {suit = Spade; value = 4};
  {suit = Heart; value = 14};
  {suit = Heart; value = 2};
  {suit = Heart; value = 7};
  {suit = Club; value = 3};
  {suit = Club; value = 13};
  {suit = Club; value = 5};
  {suit = Diamond; value = 7};
  {suit = Diamond; value = 8};
  {suit = Diamond; value = 9};
]

let hand_wo_clubs = [
  {suit = Spade; value = 3};
  {suit = Spade; value = 13};
  {suit = Spade; value = 12};
  {suit = Spade; value = 4};
  {suit = Heart; value = 14};
  {suit = Heart; value = 2};
  {suit = Heart; value = 7};
  {suit = Diamond; value = 3};
  {suit = Diamond; value = 13};
  {suit = Diamond; value = 5};
  {suit = Diamond; value = 7};
  {suit = Diamond; value = 8};
  {suit = Diamond; value = 9};
]

let hard_ai_akq = {
  hand = akq_hand;
	game_points = 0;
	ai_level = 3;
	collected_cards = [];
  p_num = 0;
}

let hard_ai_no_clubs = {
  hand = hand_wo_clubs;
	game_points = 0;
	ai_level = 3;
	collected_cards = [];
  p_num = 0;
}

let data = {
  q_spades_played = false;
  hearts_played = false;
  players = [{
    has_clubs = true;
    has_spades = true;
    has_diamonds = true;
    has_hearts = true;
    shooting_moon = false;
    tricks = [];
    round_points = 0;
  }; {
    has_clubs = true;
    has_spades = true;
    has_diamonds = true;
    has_hearts = true;
    shooting_moon = false;
    tricks = [];
    round_points = 0;
  }; {
    has_clubs = true;
    has_spades = true;
    has_diamonds = true;
    has_hearts = true;
    shooting_moon = false;
    tricks = [];
    round_points = 0;
  }; {
    has_clubs = true;
    has_spades = true;
    has_diamonds = true;
    has_hearts = true;
    shooting_moon = false;
    tricks = [];
    round_points = 0;
  }]
}

let test_pool = [{suit = Club; value = 6}; {suit = Diamond; value = 4}]

let test_pool_same_suit = [{suit = Club; value = 12}; {suit = Club; value = 13}]

let tests = "test suite" >::: [
  "pass_akq_hand"  >::
    (fun _ -> assert_equal
      (List.sort (compare_cards true) [
        {suit = Spade; value = 14};
        {suit = Spade; value = 13};
        {suit = Club; value = 13}
      ])
      (List.sort (compare_cards true) (pass_cards hard_ai_akq)));

  "pass_no_clubs_hand"  >::
    (fun _ -> assert_equal
      (List.sort (compare_cards true) [
        {suit = Spade; value = 13};
        {suit = Heart; value = 2};
        {suit = Heart; value = 7};
      ])
      (List.sort (compare_cards true) (pass_cards hard_ai_no_clubs)));

  "play_no_clubs_hand"  >::
    (fun _ -> assert_equal
      ({suit = Spade; value = 13})
      (guess_turn hard_ai_no_clubs [] data));

  "play_no_clubs_hand_full_pool"  >::
    (fun _ -> assert_equal
      ({suit = Diamond; value = 3})
      (guess_turn hard_ai_no_clubs [
        {suit = Club; value = 7};
        {suit = Diamond; value = 6};
        {suit = Diamond; value = 10};
      ] data));

  "play_akq_hand"  >::
    (fun _ -> assert_equal
      ({suit = Club; value = 13})
      (guess_turn hard_ai_akq [] data));

  "test_get_index" >::
    (fun _ -> assert_equal
      (4)
      (Types.get_index (Heart, 14) akq_hand));

  "test_highest_card_so_far" >::
    (fun _ -> assert_equal
      ({suit = Club; value = 6})
      (highest_card_so_far test_pool Club));

  "test_highest_card_so_far_same_suit" >::
    (fun _ -> assert_equal
      ({suit = Club; value = 13})
      (highest_card_so_far test_pool_same_suit Club));
]

let _ = run_test_tt_main tests
