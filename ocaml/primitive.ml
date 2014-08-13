open Qptypes;;
open Core.Std;;

type t =
{ sym  : Symmetry.t ;
  expo : Positive_float.t ;
}

let to_string p = 
  let { sym = s ; expo = e } = p in
  Printf.sprintf "(%s, %f)" 
   (Symmetry.to_string s)
   (Positive_float.to_float e)
;;
  
