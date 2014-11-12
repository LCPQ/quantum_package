type t =
{ sym : Symmetry.t;
  expo : Qptypes.AO_expo.t;
} with sexp

(** Conversion to string for printing *)
val to_string : t -> string

(** Creation *)
val of_sym_expo : Symmetry.t -> Qptypes.AO_expo.t -> t

