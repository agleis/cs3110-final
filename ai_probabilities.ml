let rec create_card_list suit val lst =
  if val < 2
  then lst
  else create_card_list suit (val-1) ({suit = suit; value = val}::lst)

let losing_cards high_card =
  create_card_list high_card.suit high_card.value []
