exception GTO_Read_Failure of string
exception End_Of_Basis
type t =
  { sym : Symmetry.t ;
    lc  : (Primitive.t * Qptypes.AO_coef.t) list;
  } with sexp

(** Create from a list of Primitive.t * Qptypes.AO_coef.t *)
val of_prim_coef_list :
  (Primitive.t * Qptypes.AO_coef.t) list -> t

(** Read from a file *)
val read_one : in_channel -> t

(** Convert to string for printing *)
val to_string : t -> string
