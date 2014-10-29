open Core.Std;;

type t =
| Guess
| Canonical
| Natural
| Localized
| None
with sexp
;;

let to_string = function
  | Guess     -> "Guess"
  | Canonical -> "Canonical"
  | Natural   -> "Natural"
  | Localized -> "Localized"
  | None      -> "None"
;;

let of_string  s = 
  match String.lowercase s with 
  | "guess"     -> Guess
  | "canonical" -> Canonical
  | "natural"   -> Natural
  | "localized" -> Localized
  | "none"      -> None
  | _ -> failwith "MO_label should be one of:
    Guess | Canonical | Natural | Localized | None."
;;
