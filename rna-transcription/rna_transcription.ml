type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna dna =
  let dna_converter y =
    match y with
    | `A -> `U
    | `C -> `G
    | `G -> `C
    | `T -> `A
  in
  List.map dna_converter dna
