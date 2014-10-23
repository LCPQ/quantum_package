open Qptypes;;
open Core.Std;;

type t =
{ sym  : Symmetry.t ;
  expo : AO_expo.t ;
}

let to_string p = 
  let { sym = s ; expo = e } = p in
  Printf.sprintf "(%s, %f)" 
   (Symmetry.to_string s)
   (AO_expo.to_float e)
;;
  
