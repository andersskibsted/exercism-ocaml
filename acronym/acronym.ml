let is_alpha_or_ws c =
  Base.Char.is_alpha c || Base.Char.is_whitespace c

let rec acro w =
  match w with
  | [] -> []
  | ' ' :: (x :: xs) -> x :: acro xs
  | _ :: xs -> acro xs

let acronym word =
  let w = String.map (fun c -> if c = '-' then ' ' else c) word
  in
  let acron = String.to_seq w
  |> Seq.filter is_alpha_or_ws
  |> List.of_seq
  |> acro
  in
  List.map Base.Char.uppercase (word.[0] :: acron)
  |> List.filter (fun c -> not @@ Base.Char.is_whitespace c)
  |> Base.String.of_char_list
