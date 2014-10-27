open Qptypes;;
open Core.Std;;

type t =
{ sym  : Symmetry.t ;
  expo : AO_expo.t ;
} with sexp

let to_string p = 
  let { sym = s ; expo = e } = p in
  Printf.sprintf "(%s, %f)" 
   (Symmetry.to_string s)
   (AO_expo.to_float e)
;;
  
let of_sym_expo s e = 
  { sym=s ; expo=e}
;;
