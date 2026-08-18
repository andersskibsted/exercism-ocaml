let reverse_string str =
  let char_list = List.init (String.length str) (fun i -> str.[i])
  in
  List.fold_right (fun s acc -> acc ^ String.make 1 s ) char_list ""
