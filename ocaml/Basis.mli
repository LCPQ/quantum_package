open Qptypes;;

type t = (Gto.t * Atom_number.t) list

(** Read all the basis functions of an element and set the number of the
  * atom *)
val read : in_channel -> Atom_number.t -> (Gto.t * Atom_number.t) list

(** Find an element in the basis set file *)
val find : in_channel -> Element.t -> Element.t

(** Read the basis of an element from the file *)
val read_element :
  in_channel -> Atom_number.t -> Element.t -> (Gto.t * Atom_number.t) list

(** Convert the basis to a string *)
val to_string :  (Gto.t * Atom_number.t) list -> string
