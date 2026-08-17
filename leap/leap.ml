let leap_year year =
  match year with
  | y when y mod 4 <> 0 -> false
  | y when y mod 4 = 0 && y mod 400 = 0 -> true
  | y when y mod 4 = 0 && y mod 100 = 0 -> false
  | _ -> true
