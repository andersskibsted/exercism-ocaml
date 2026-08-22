let primes top =
  let rec sieve lst =
    match lst with
    | [] -> []
    | x :: xs when x * x > top -> x :: xs
    | x :: xs -> x :: sieve (List.filter (fun y -> y mod x <> 0) xs)
in
if top < 2 then []
else
  sieve (List.init (top - 1) (fun i -> i + 2))

