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

test_cis ();;
