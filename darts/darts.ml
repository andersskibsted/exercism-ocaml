
let distance_to_center x y =
  sqrt ((x *. x) +. (y *. y))

let score (x: float) (y: float): int =
  match distance_to_center x y with
  | d when d > 10.0 -> 0
  | d when d > 5.0 -> 1
  | d when d > 1.0 -> 5
  | _ -> 10
