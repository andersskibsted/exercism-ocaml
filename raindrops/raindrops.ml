let raindrop num =
  let sounds = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
  in
  let result = List.fold_left (fun acc (x, str) -> if num mod x = 0 then acc ^ str else acc) "" sounds
  in
  if result = "" then string_of_int num else result
