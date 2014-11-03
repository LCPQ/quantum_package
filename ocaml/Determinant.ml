open Core.Std;;
open Qptypes;;

type t = int64 array with sexp

let to_int64_array (x:t) = (x:int64 array)
;;

let to_alpha_beta x = 
  let x = to_int64_array x in
  let n_int = (Array.length x)/2 in
  ( Array.init n_int ~f:(fun i -> x.(i)) ,
    Array.init n_int ~f:(fun i -> x.(i+n_int)) )
;;

let to_bitlist_couple x =
  let (xa,xb) = to_alpha_beta x in
  let xa = to_int64_array xa
  |> Array.to_list
  |> Bitlist.of_int64_list
  and xb = to_int64_array xb
  |> Array.to_list
  |> Bitlist.of_int64_list
  in (xa,xb)
;;

let bitlist_to_string ~mo_tot_num x =
  List.map x ~f:(fun i -> match i with
    | Bit.Zero -> "-"
    | Bit.One  -> "+" )
  |> String.concat
  |> String.sub ~pos:0 ~len:(MO_number.to_int mo_tot_num) 
;;


let of_int64_array ~n_int ~alpha ~beta x =
   assert ((Array.length x) = (N_int_number.to_int n_int)*2) ;
   let (a,b) = to_bitlist_couple x 
   and alpha = Elec_alpha_number.to_int alpha
   and beta  = Elec_beta_number.to_int beta
   in
   if ( (Bitlist.popcnt a) <> alpha) then
     begin
       let mo_tot_num = MO_number.get_max () in
       let mo_tot_num = MO_number.of_int mo_tot_num ~max:mo_tot_num  in
       failwith (Printf.sprintf "Expected %d electrons in alpha determinant
%s" alpha (bitlist_to_string ~mo_tot_num:mo_tot_num a) )
     end;
   if ( (Bitlist.popcnt b) <> beta ) then
     begin
       let mo_tot_num = MO_number.get_max () in
       let mo_tot_num = MO_number.of_int mo_tot_num ~max:mo_tot_num  in
       failwith (Printf.sprintf "Expected %d electrons in beta determinant
%s" beta (bitlist_to_string ~mo_tot_num:mo_tot_num b) )
     end;
   x
;;

let of_bitlist_couple ~alpha ~beta (xa,xb) =
  let ba = Bitlist.to_int64_list xa in
  let bb = Bitlist.to_int64_list xb in
  let n_int = Bitlist.n_int_of_mo_tot_num (List.length xa) in
  of_int64_array ~n_int:n_int ~alpha:alpha ~beta:beta (Array.of_list (ba@bb))
;;

let to_string ~mo_tot_num x = 
  let (xa,xb) = to_bitlist_couple x in
  [ bitlist_to_string ~mo_tot_num:mo_tot_num xa ;
    bitlist_to_string ~mo_tot_num:mo_tot_num xb ]
  |> String.concat ~sep:"\n"
;;


