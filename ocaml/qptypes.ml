module Positive_float : sig
  type t
  val to_float : t -> float
  val of_float : float -> t
end = struct
  type t = float
  let to_float x = x
  let of_float x = ( assert (x >= 0.) ; x )
end


module Strictly_positive_float : sig
  type t
  val to_float : t ->float 
  val of_float : float -> t
end = struct
  type t =float 
  let to_float x = x
  let of_float x = ( assert (x > 0.) ; x )
end


module Positive_int : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let to_int x = x
  let of_int x = ( assert (x >= 0) ; x )
end


module Strictly_positive_int : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let to_int x = x
  let of_int x = ( assert (x > 0) ; x )
end


module Non_empty_string : sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = string
  let to_string x = x
  let of_string x = ( assert (x <> "") ; x )
end

(*
module MO_number : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let to_int x = x
  let of_int x = ( assert (x > 0) ;
  if (Ezfio.has_mo_basis_mo_tot_num ()) then
    assert (x <= (Ezfio.get_mo_basis_mo_tot_num ())); x )
end


module AO_number : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let to_int x = x
  let of_int x = ( assert (x > 0) ;
  if (Ezfio.has_ao_basis_ao_num ()) then
    assert (x <= (Ezfio.get_ao_basis_ao_num ())); x )
end


module N_int_number : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let to_int x = x
  let of_int x = ( assert (x > 0) ;
  if (Ezfio.has_determinants_n_int ()) then
    assert (x == (Ezfio.get_determinants_n_int ())); x )
end

module Det_number : sig
  type t
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int
  let to_int x = x
  let of_int x = ( assert (x > 0) ;
  if (Ezfio.has_determinants_det_num ()) then
    assert (x <= (Ezfio.get_determinants_det_num ())); x )
end
*)
