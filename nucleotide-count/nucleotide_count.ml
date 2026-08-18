open Base

let empty = Map.empty (module Char)

let increment c map =
  Map.update map c
    ~f:(function
      | None -> 1
      | Some n -> n + 1)

let is_nucl c =
  List.mem ['A'; 'G'; 'C'; 'T'] c ~equal:Char.equal

let count_nucleotide s c =
  if not (is_nucl c) then Error c
  else
    let rec aux (acc: int) (str: char list) (ch: char) =
      match str, ch with
      | [], _ -> Ok acc
      | x :: xs, ch when is_nucl x ->  aux (if Char.equal x ch then acc + 1 else acc) xs ch
      | c :: _, _ -> Error c
    in
    let ch_lst = List.init (String.length s) ~f:(fun i -> s.[i]) in
    aux 0 ch_lst c


let count_nucleotides s =
  let char_list = List.init (String.length s) ~f:(String.get s) in
  let rec aux acc ch_lst =
    match ch_lst with
    | [] -> Ok acc
    | x :: xs when is_nucl x -> aux (increment x acc) xs
    | c :: _ -> Error c
  in
  aux empty char_list
