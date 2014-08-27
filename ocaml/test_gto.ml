open Core.Std;;
open Qptypes;;

let test_prim () = 
  let p = 
   { Primitive.sym  = Symmetry.P ; 
     Primitive.expo = Positive_float.of_float 0.15} in
  Primitive.to_string p
  |> print_string
;;
  
let test_gto_1 () =
  let in_channel = open_in "/home/scemama/quantum_package/data/basis/cc-pvdz" in
  ignore (input_line in_channel);
  let gto = Gto.read_one in_channel in
  print_string (Gto.to_string gto);
  let gto = Gto.read_one in_channel in
  print_string (Gto.to_string gto);
  let gto = Gto.read_one in_channel in
  print_string (Gto.to_string gto);
;;

let test_gto_2 () =
  let in_channel = open_in "/home/scemama/quantum_package/data/basis/cc-pvdz" in
  ignore (input_line in_channel);
  let basis = Basis.read in_channel in
  List.iter basis ~f:(fun x-> Printf.printf "%s\n" (Gto.to_string x))
;;

let test_gto () =
  let in_channel = open_in "/home/scemama/quantum_package/data/basis/cc-pvdz" in
  let basis = Basis.read_element in_channel Element.C in
  List.iter basis ~f:(fun x-> Printf.printf "%s\n" (Gto.to_string x))
;;

let test_module () =
  test_gto()
;;

test_module ();;
