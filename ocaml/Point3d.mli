type t =
{ x : float;
  y : float;
  z : float;
} with sexp

(** Create from an xyz string *)
val of_string : Units.units -> string -> t

(** Convert to a string for printing *)
val to_string : Units.units -> t -> string

(** Computes the squared distance between 2 points *)
val distance2 : t -> t -> Qptypes.Positive_float.t

(** Computes the distance between 2 points *)
val distance : t -> t -> float
