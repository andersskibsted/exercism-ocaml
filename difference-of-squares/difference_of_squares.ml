let sum_of_n n =
  n*(n+1)/2

let rec pow x n =
  if n = 0 then 1
    else x * pow x (n-1)

let square_of_sum n =
  pow (sum_of_n n) 2

let sum_of_squares n =
  List.init n (fun i -> i+1)
  |> List.map (fun x -> x*x)
  |> List.fold_left (fun acc x -> acc + x) 0

let difference_of_squares n =
  square_of_sum n - sum_of_squares n
