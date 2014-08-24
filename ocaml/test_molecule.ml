open Core.Std ;;
open Qptypes ;;

let test_molecule () =
  let xyz =
"
H           1.0       0.54386314      0.00000000     -3.78645152
O           8.0       1.65102147      0.00000000     -2.35602344
H           1.0       0.54386314      0.00000000     -0.92559535
"
  in
  
  try ignore (Molecule.of_xyz_string xyz 1 (Strictly_positive_int.of_int 1))
    with
    | Molecule.MultiplicityError _ -> print_string "OK\n"
  ;
  print_string "---\n";
  let m = Molecule.of_xyz_string xyz 0 (Strictly_positive_int.of_int 1)
  in print_endline (Molecule.name m) ;
  let m = Molecule.of_xyz_string xyz 1 (Strictly_positive_int.of_int 2)
  in print_endline (Molecule.name m) ;

  let xyz =
"
H        0.54386314      0.00000000     -3.78645152
O        1.65102147      0.00000000     -2.35602344
H        0.54386314      0.00000000     -0.92559535
"
  in
  let m = Molecule.of_xyz_string xyz (-2) (Strictly_positive_int.of_int 1)
  in print_endline (Molecule.name m) ;
  print_string (Molecule.to_string m);
;;

test_molecule ();;
