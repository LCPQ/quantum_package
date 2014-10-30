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

* Det_coef : float
  assert (x >= -1.) ; 
  assert (x <=  1.) ; 

* Normalized_float : float
  assert (x <= 1.) ; 
  assert (x >= 0.) ; 

* Strictly_negative_int : int  
  assert (x < 0) ; 

* Non_empty_string : string
  assert (x <> \"\") ;


* AO_number : int  
  assert (x > 0) ; 
  if (x > 1000) then
    warning \"More than 1000 AOs\";
  if (Ezfio.has_ao_basis_ao_num ()) then
    assert (x <= (Ezfio.get_ao_basis_ao_num ()));

* Nucl_number : int  
  assert (x > 0) ; 
  if (x > 1000) then
    warning \"More than 1000 atoms\";
  if (Ezfio.has_nuclei_nucl_num ()) then
    assert (x <= (Ezfio.get_nuclei_nucl_num ()));

* N_int_number : int 
  assert (x > 0) ; 
  if (x > 100) then
    warning \"N_int > 100\";
  if (Ezfio.has_determinants_n_int ()) then
    assert (x = (Ezfio.get_determinants_n_int ()));

* Det_number : int 
  assert (x > 0) ; 
  if (x > 100000000) then
    warning \"More than 100 million determinants\";
  if (Ezfio.has_determinants_n_det ()) then
    assert (x <= (Ezfio.get_determinants_n_det ()));

* Det_number_max : int 
  assert (x > 0) ; 
  if (x > 100000000) then
    warning \"More than 100 million determinants\";

* States_number : int 
  assert (x > 0) ; 
  if (x > 100) then
    warning \"More than 100 states\";
  if (Ezfio.has_determinants_n_states_diag ()) then
    assert (x <= (Ezfio.get_determinants_n_states_diag ()))
  else if (Ezfio.has_determinants_n_states ()) then
    assert (x <= (Ezfio.get_determinants_n_states ()));

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

* Bitmask_number : int
  assert (x > 0) ;

* MO_coef : float

* MO_occ : float
  assert (x >= 0.); 

* AO_coef : float

* AO_expo : float  
  assert (x >= 0.) ; 

* AO_prim_number : int
  assert (x > 0) ;

* Threshold : float
  assert (x >= 0.) ;
  assert (x <= 1.) ;

* PT2_energy : float
  assert (x >=0.) ;

* Elec_alpha_number : int
  assert (x > 0) ;

* Elec_beta_number : int
  assert (x >= 0) ;

* Elec_number : int
  assert (x > 0) ;

* MD5 : string
  assert ((String.length x) = 32);

* Rst_string : string

"
;;

let untouched = "
module MO_number : sig
  type t with sexp
  val to_int : t -> int
  val get_mo_tot_num : unit -> int 
  val of_int : ?mo_tot_num:int -> int -> t
  val to_string : t -> string
end = struct
  type t = int with sexp
  let get_mo_tot_num () =
    if (Ezfio.has_mo_basis_mo_tot_num ()) then
      Ezfio.get_mo_basis_mo_tot_num ()
    else
      0
  let to_int x = x
  let of_int  ?mo_tot_num  x = ( assert (x > 0) ;
  if (x > 1000) then
    warning \"More than 1000 MOs\";
  let mo_tot_num = match mo_tot_num with
  | Some i -> i
  | None -> get_mo_tot_num ()
  in
  begin
    match mo_tot_num with
    | 0 -> ()
    | i -> assert ( x <= i )
  end
  ; x )

  let to_string x = Int.to_string x
end

"

let template = format_of_string "
module %s : sig
  type t with sexp
  val to_%s : t -> %s
  val of_%s : %s %s -> t
  val to_string : t -> string
end = struct
  type t = %s with sexp
  let to_%s x = x
  let of_%s %s x = ( %s x )
  let to_string x = %s.to_string x
end

"
;;

let parse_input input=
  print_string "open Core.Std;;\nlet warning = print_string;;\n" ;
  let rec parse result = function
    | [] -> result
    | ( "" , ""   )::tail -> parse result tail
    | ( t  , text )::tail -> 
        let name,typ,params,params_val = 
          match String.split ~on:':' t with
          | [name;typ] -> (name,typ,"","")
          | name::typ::params::params_val -> (name,typ,params,
            (String.concat params_val ~sep:":") )
          | _ -> assert false
        in
        let typ  = String.strip typ
        and name = String.strip name in
        let typ_cap = String.capitalize typ in
        let newstring = Printf.sprintf template name typ typ typ params_val typ typ 
          typ typ params ( String.strip text ) typ_cap
        in
        List.rev (parse (newstring::result) tail )
  in
     String.split ~on:'*' input
  |> List.map ~f:(String.lsplit2_exn ~on:'\n') 
  |> parse []
  |> String.concat 
  |> print_string
;;

let () = 
  parse_input input_data ;
  print_endline untouched
;;


