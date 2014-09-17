open Core.Std;;
open Qputils;;

let test_module () =
  
    let basis_channel =
      let b = "cc-pvdz" in
      In_channel.create (Qpackage.root / "data/basis" / (String.lowercase b))
    in

    let molecule =
      let xyz_file = "F2.xyz" in
      Molecule.of_xyz_file xyz_file 
    in

    let basis = 
      (Basis.read_element basis_channel Element.F) @ 
      (Basis.read_element basis_channel Element.F) 
    in

    Long_basis.of_basis basis
    |> Long_basis.to_string 
    |> print_endline
;;

test_module ();
