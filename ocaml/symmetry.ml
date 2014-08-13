type t = S|P|D|F|G|H|I|J|K|L

let to_string = function
  | S -> "S"
  | P -> "P"
  | D -> "D"
  | F -> "F"
  | G -> "G"
  | H -> "H"
  | I -> "I"
  | J -> "J"
  | K -> "K"
  | L -> "L"

let of_string = function
  | "S" -> S
  | "P" -> P
  | "D" -> D
  | "F" -> F
  | "G" -> G
  | "H" -> H
  | "I" -> I
  | "J" -> J
  | "K" -> K
  | "L" -> L
  | x -> raise (Failure ("Symmetry should be S|P|D|F|G|H|I|J|K|L,
not "^x^"."))

let of_char = function
  | 'S' -> S
  | 'P' -> P
  | 'D' -> D
  | 'F' -> F
  | 'G' -> G
  | 'H' -> H
  | 'I' -> I
  | 'J' -> J
  | 'K' -> K
  | 'L' -> L
  | x -> raise (Failure ("Symmetry should be S|P|D|F|G|H|I|J|K|L"))
