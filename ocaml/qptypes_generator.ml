open Core.Std;;

let input_data = "
* Positive_float : float  
  assert (x >= 0.) ; 

* Strictly_positive_float : float  
  assert (x > 0.) ; 

* Negative_float : float  
  assert (x <= 0.) ; 

* Strictly_negative_float : float  
  assert (x < 0.) ; 

* Positive_int : int  
  assert (x >= 0) ; 

* Strictly_positive_int : int  
  assert (x > 0) ; 

* Negative_int : int  
  assert (x <= 0) ; 

* Strictly_negative_int : int  
  assert (x < 0) ; 

* Non_empty_string : string
  assert (x <> \"\") ;

* MO_number : int  
  assert (x > 0) ; 
  if (x > 1000) then
    warning \"More than 1000 MOs\";
  if (Ezfio.has_mo_basis_mo_tot_num ()) then
    assert (x <= (Ezfio.get_mo_basis_mo_tot_num ()));

* AO_number : int  
  assert (x > 0) ; 
  if (x > 1000) then
    warning \"More than 1000 AOs\";
  if (Ezfio.has_ao_basis_ao_num ()) then
    assert (x <= (Ezfio.get_ao_basis_ao_num ()));

* N_int_number : int 
  assert (x > 0) ; 
  if (x > 100) then
    warning \"N_int > 100\";
  if (Ezfio.has_determinants_n_int ()) then
    assert (x == (Ezfio.get_determinants_n_int ()));

* Det_number : int 
  assert (x > 0) ; 
  if (x > 100000000) then
    warning \"More than 100 million determinants\";
  if (Ezfio.has_determinants_det_num ()) then
    assert (x <= (Ezfio.get_determinants_det_num ()));

* Bit_kind_size : int  
  begin match x with
  | 8 | 16 | 32 | 64 -> ()
  | _ -> raise (Failure \"Bit_kind_size should be (8|16|32|64).\")
  end;

* Bit_kind : int  
  begin match x with
  | 1 | 2 | 4 | 8 -> ()
  | _ -> raise (Failure \"Bit_kind should be (1|2|4|8).\")
  end;

"
;;


let template = format_of_string "
module %s : sig
  type t
  val to_%s : t -> %s
  val of_%s : %s -> t
end = struct
  type t = %s
  let to_%s x = x
  let of_%s x = ( %s x )
end

"
;;

let parse_input input=
  print_string "let warning = print_string;;\n" ;
  let rec parse result = function
    | [] -> result
    | ( "" , ""   )::tail -> parse result tail
    | ( t  , text )::tail -> 
        let  name , typ  = String.lsplit2_exn ~on:':' t
        in
        let typ  = String.strip typ
        and name = String.strip name
        in
        let newstring = Printf.sprintf template name typ typ typ typ typ typ typ
        ( String.strip text )
        in
        List.rev (parse (newstring::result) tail )
  in
     String.split ~on:'*' input
  |> List.map ~f:(String.lsplit2_exn ~on:'\n') 
  |> parse []
  |> String.concat 
  |> print_string
;;

let () = parse_input input_data;;


