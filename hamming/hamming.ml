type nucleotide = A | C | G | T

let hamming_distance seq1 seq2 =
  let rec aux acc s1 s2 =
    match s1, s2 with
    | [], [] -> Ok acc
    | x :: xs, y :: ys ->
      aux (if x <> y then acc + 1 else acc) xs ys
    | _ -> Error "strands must be of equal length"
  in
  aux 0 seq1 seq2
