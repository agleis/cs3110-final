open OUnit2
open Types
open Randoms
open Ai_controller

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

let hard_ai_akq = {
  hand = akq_hand;
	round_points = 0;
	game_points = 0;
	ai_level = 3;
	collected_cards = [];
  p_num = 0;
}

let tests = "test suite" >::: [
  "pass_akq_hand"  >::
    (fun _ -> assert_equal
      (List.sort (compare_cards true) [
        {suit = Spade; value = 14};
        {suit = Spade; value = 13};
        {suit = Club; value = 13}
      ])
      (List.sort (compare_cards true) (pass_cards hard_ai_akq)));

  "test_get_index" >::
    (fun _ -> assert_equal
      (4)
      (Types.get_index (Heart, 14) akq_hand));
]

let _ = run_test_tt_main tests
