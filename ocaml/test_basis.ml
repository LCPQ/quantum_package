open Core.Std;;
open Qputils;;
open Qptypes;;

let test_module () =
  
    let basis_channel =
      let b = "cc-pvdz" in
      In_channel.create (Qpackage.root / "data/basis" / (String.lowercase b))
    in

    let molecule =
      let xyz_file = "F2.xyz" in
      Molecule.of_xyz_file xyz_file 
    in

    let nuclei = molecule.Molecule.nuclei in

    let basis = 
      (Basis.read_element basis_channel (Atom_number.of_int 1) Element.F) @ 
      (Basis.read_element basis_channel (Atom_number.of_int 2) Element.F) 
    in

    Long_basis.of_basis basis 
    |> Long_basis.to_string 
    |> print_endline
;;

test_module ();
