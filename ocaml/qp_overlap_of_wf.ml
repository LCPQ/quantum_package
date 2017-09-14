(** 
 * Computes the overlap <Psi_0 | Psi_1> where both Psi_0 and Psi_1 are truncated in the set
 * of common determinants and normalized
 *)

open Input_determinants_by_hand
open Qptypes


let () =
  let ezfio, ezfio' =
    try
      Sys.argv.(1), Sys.argv.(2)
    with Invalid_argument _ ->
      raise (Invalid_argument (Printf.sprintf
         "Syntax : %s EZFIO1 EZFIO2" Sys.argv.(0)))
  in

  let fetch_wf filename =
    Ezfio.set_file filename;
    let mo_tot_num =
      Ezfio.get_mo_basis_mo_tot_num ()
      |> MO_number.of_int
    in
    let d =
      Determinants_by_hand.read ()
    in
    let n_det =
      Det_number.to_int d.Determinants_by_hand.n_det
    in
    let keys = 
      Array.map (Determinant.to_string ~mo_tot_num) 
        d.Determinants_by_hand.psi_det
    and values =
      Array.map Det_coef.to_float
        d.Determinants_by_hand.psi_coef
    in
    let hash = 
      Hashtbl.create n_det
    in
    for i=0 to n_det-1
    do
      Hashtbl.add hash keys.(i) values.(i);
    done;
    hash
  in

  let overlap wf wf' =
    let result, norm, norm' = 
      Hashtbl.fold (fun k c (accu,norm,norm') ->
        let (c',c) =
          try  (Hashtbl.find wf' k, c)
          with Not_found -> (0.,0.)
        in
        (accu +. c *. c' , 
        norm +. c *. c  , 
        norm'+. c'*. c' ) 
      ) wf (0.,0.,0.)
    in 
    result /. (sqrt (norm *. norm'))
  in

  let wf, wf' = 
     fetch_wf ezfio,
     fetch_wf ezfio'
  in

  let o = 
    overlap wf wf'
  in
  print_float (abs_float o) ;
  print_newline ()
  
