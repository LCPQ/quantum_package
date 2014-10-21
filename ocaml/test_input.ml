let test_ao () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Ao_basis.read ()
  in
  print_endline (Input.Ao_basis.to_string b);
;;

let test_bielec_intergals () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Bielec_integrals.read ()
  in
  print_endline (Input.Bielec_integrals.to_string b);
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
;;

let test_electrons () =
  Ezfio.set_file "F2.ezfio" ;
  let b = Input.Electrons.read ()
  in
  print_endline (Input.Electrons.to_string b);
;;

test_electrons();;
