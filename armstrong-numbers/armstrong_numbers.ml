(* type candidate = int  *)
let rec pow x n =
  match (x, n) with
  | (_, n) when n = 0 -> 1
  | (x, n) -> x * pow x (n-1)

let char_to_digit c =
  int_of_char c - int_of_char '0'

let validate candidate =
  let str = Int.to_string candidate in
  let num_of_digits = String.length str in
  let ciffre = String.to_seq str |> List.of_seq |> List.map char_to_digit in
  let sumsq = List.fold_left (fun acc x -> acc + (pow x num_of_digits)) 0 ciffre in
    candidate = sumsq
