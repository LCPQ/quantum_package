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
  let rst = Input.Bielec_integrals.to_rst b in
  let b2 = Input.Bielec_integrals.of_rst rst in
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "rst failed";
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
  print_endline (Input.Determinants.to_string b);
;;

let test_cisd_sc2 () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Cisd_sc2.read ()
  in
  print_endline (Input.Cisd_sc2.to_string b);
  let rst = Input.Cisd_sc2.to_rst b in
  let b2 = Input.Cisd_sc2.of_rst rst in
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "rst failed";

;;

let test_electrons () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Electrons.read ()
  in
  print_endline (Input.Electrons.to_string b);
  let rst = Input.Electrons.to_rst b in
  let new_b = Input.Electrons.of_rst rst in
  if (b = new_b) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

let test_fci () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Full_ci.read ()
  in
  print_endline (Input.Full_ci.to_string b);
  let rst = Input.Full_ci.to_rst b in
  let new_b = Input.Full_ci.of_rst rst in
  print_endline (Input.Full_ci.to_string b);
  if (b = new_b) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

let test_hf () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Hartree_fock.read ()
  in
  print_endline (Input.Hartree_fock.to_string b);
  let rst = Input.Hartree_fock.to_rst b in
  let new_b = Input.Hartree_fock.of_rst rst in
  print_endline (Input.Hartree_fock.to_string b);
  if (b = new_b) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

let test_mo () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Mo_basis.read ()
  in
  print_endline (Input.Mo_basis.to_string b);
;;

let test_nucl () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Nuclei.read () in
  let rst = Input.Nuclei.to_rst b in
  let new_b = Input.Nuclei.of_rst rst in
  print_endline (Input.Nuclei.to_string b);
  if (b = new_b) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

(*
test_ao ();;
test_bitmasks ();
test_cis ();
test_cisd_sc2 ();
test_dets ();
test_hf ();;
test_mo ();;
test_nucl ();
test_bielec_intergals ();;
test_electrons();
*)

test_ao();
