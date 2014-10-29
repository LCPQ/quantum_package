open Qptypes;;

let test_module () = 
  let mo_tot_num = MO_number.of_int 10 in
  let det =
   [| 15L ; 7L |]
   |> Determinant.of_int64_array (N_int_number.of_int 1)
  in
  Printf.printf "%s\n" (Determinant.to_string (~mo_tot_num:mo_tot_num) det)
;;

test_module ();;
