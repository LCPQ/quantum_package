open Qptypes;;

type t = S | P | D | F | G | H | I | J | K | L
val to_string : t -> string
val of_string : string -> t
val of_char : char -> t
val to_l : t -> Positive_int.t
type st = t
module Xyz :
  sig
    type t = { x: Positive_int.t ;
               y: Positive_int.t ;
               z: Positive_int.t }
    val of_string : string -> t
    val to_string : t -> string
    val get_l : t -> Positive_int.t
    val of_symmetry : st -> t list
  end
