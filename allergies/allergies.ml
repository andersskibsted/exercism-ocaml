type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
let allergen_bit = function
  | Eggs -> 0
  | Peanuts -> 1
  | Shellfish -> 2
  | Strawberries -> 3
  | Tomatoes -> 4
  | Chocolate -> 5
  | Pollen -> 6
  | Cats -> 7


let allergic_to n allerg =
  n land (1 lsl allergen_bit allerg) <> 0

let allergies score =
  let allergies = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats]
  in
  List.filter (allergic_to score) allergies
