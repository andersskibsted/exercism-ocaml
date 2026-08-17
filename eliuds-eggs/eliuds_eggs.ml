let rec pow x n =
  if n = 0 then 1
    else x * pow x (n-1)
let rec convert_to_binary_list number p =
  match number with
  | number when number = 1 -> List.([1])
  | number when number = 0 -> List.([0])
  | number when number mod (pow 2 p) = 0 -> 0 :: convert_to_binary_list (number) (p+1)
  | number -> let remainder = number mod (pow 2 p) in 1 :: convert_to_binary_list (number - remainder) (p+1)

let egg_count number =
  List.fold_left (fun acc x -> acc + x) 0 (convert_to_binary_list number 1)
