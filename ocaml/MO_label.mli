type t =
  | Guess
  | Canonical
  | Natural
  | Localized
  | None
with sexp

(** String representation *)
val to_string : t -> string

(** Build from string representation *)
val of_string : string -> t

