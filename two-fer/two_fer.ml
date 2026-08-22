let two_fer name =
  match name with
  | Some n -> "One for " ^ n ^ ", one for me."
  | None -> "One for you, one for me."
