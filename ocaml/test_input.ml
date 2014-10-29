open Qptypes;;

let test_ao () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Ao_basis.read ()
  in
  print_endline (Input.Ao_basis.to_string b);
  print_endline (Input.Ao_basis.to_rst b |> Rst_string.to_string);
;;

let test_bielec_intergals () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Bielec_integrals.read ()
  in
  let output = Input.Bielec_integrals.to_string b
  in
  print_endline output;
;;

let test_bitmasks () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Bitmasks.read ()
  in
  print_endline (Input.Bitmasks.to_string b);
;;

let test_cis () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Cis_dressed.read ()
  in
  print_endline (Input.Cis_dressed.to_string b);
;;

let test_dets () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Determinants.read ()
  in
  (*print_endline (Input.Determinants.debug b);*)
  print_endline (Input.Determinants.to_string b);
;;

let test_cisd_sc2 () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Cisd_sc2.read ()
  in
  print_endline (Input.Cisd_sc2.to_string b);
;;

let test_electrons () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Electrons.read ()
  in
  print_endline (Input.Electrons.to_string b);
;;

let test_fci () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Full_ci.read ()
  in
  print_endline (Input.Full_ci.to_string b);
;;

let test_hf () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Hartree_fock.read ()
  in
  print_endline (Input.Hartree_fock.to_string b);
;;

let test_mo () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Mo_basis.read ()
  in
  print_endline (Input.Mo_basis.debug b);
;;

let test_nucl () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Nuclei.read ()
  in
  print_endline (Input.Nuclei.to_string b);
;;

let test_nucl_read () =
  let rst_input = Rst_string.of_string "
Molecule
========


Nuclear coordinates in xyz format (Angstroms) ::

  2
  
  F    9 0.00000000  0.00000000 -0.70000000
  F    9 0.00000000  0.00000000 0.70000000



Electrons
=========
" in
  let b = Input.Nuclei.of_rst rst_input
  in
  print_endline (Input.Nuclei.to_string b);
;;

(*
test_ao ();;
test_bielec_intergals ();;
test_bitmasks ();
test_cis ();
test_cisd_sc2 ();
test_dets ();
test_hf ();;
test_mo ();;
test_nucl ();
*)
test_nucl_read();;

