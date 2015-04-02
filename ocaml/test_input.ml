open Qptypes;;

let test_ao () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Ao_basis.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Ao_basis.to_string b);
  print_endline (Input.Ao_basis.to_rst b |> Rst_string.to_string);
;;

let test_bielec_intergals () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Bielec_integrals.read () with
  | Some x -> x
  | None   -> assert false
  in
  let output = Input.Bielec_integrals.to_string b
  in
  print_endline output;
  let rst = Input.Bielec_integrals.to_rst b in
  let b2 = match Input.Bielec_integrals.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "rst failed";
;;

let test_bitmasks () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Bitmasks.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Bitmasks.to_string b);
;;


let test_dets () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Determinants.read () with
  | Some x -> x
  | None -> assert false
  in
  print_endline (Input.Determinants.to_rst b |> Rst_string.to_string ) ;
  print_endline (Input.Determinants.sexp_of_t b |> Sexplib.Sexp.to_string ) ;
  let rst = Input.Determinants.to_rst b in
  let b2 = match Input.Determinants.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  if (b2 = b) then
    print_endline "OK"
  else
    print_endline "Failed"
;;

let test_cisd_sc2 () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Cisd_sc2_selected.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Cisd_sc2_selected.to_string b);
  let rst = Input.Cisd_sc2_selected.to_rst b in
  let b2 = match Input.Cisd_sc2_selected.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "rst failed";

;;

let test_electrons () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Electrons.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Electrons.to_string b);
  let rst = Input.Electrons.to_rst b in
  let b2 = match Input.Electrons.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

let test_fci () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Full_ci.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Full_ci.to_string b);
  let rst = Input.Full_ci.to_rst b in
  let b2 = match Input.Full_ci.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  print_endline (Input.Full_ci.to_string b);
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

let test_hf () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Hartree_fock.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Hartree_fock.to_string b);
  let rst = Input.Hartree_fock.to_rst b in
  let b2 = match Input.Hartree_fock.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  print_endline (Input.Hartree_fock.to_string b);
  if (b = b2) then
    print_endline "OK"
  else
    print_endline "Failed in rst"
;;

let test_mo () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Mo_basis.read () with
  | Some x -> x
  | None   -> assert false
  in
  print_endline (Input.Mo_basis.to_string b);
;;

let test_nucl () =
  Ezfio.set_file "F2.ezfio" ;
  let b = match Input.Nuclei.read () with
  | Some x -> x
  | None   -> assert false
  in
  let rst = Input.Nuclei.to_rst b in
  let b2 = match Input.Nuclei.of_rst rst with
  | Some x -> x
  | None -> assert false
  in
  print_endline (Input.Nuclei.to_string b);
  if (b = b2) then
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
test_dets ();

