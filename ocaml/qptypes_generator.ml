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

* Energy : float
  assert (x <=0.) ;

* S2 : float
  assert (x >=0.) ;

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
  assert (String.fold x ~init:true ~f:(fun accu x ->
     accu && (x < 'g')));

* Rst_string : string

* AO_basis_name : string
  assert (x <> \"\") ;

"
;;

let input_ezfio = "
* MO_number : int
  mo_basis_mo_tot_num
  1 : 10000
  More than 10000 MOs

* AO_number : int
  ao_basis_ao_num
  1 : 10000
  More than 10000 AOs

* Nucl_number : int
  nuclei_nucl_num
  1 : 10000
  More than 10000 nuclei

* N_int_number : int
  determinants_n_int
  1 : 30
  N_int > 30

* Det_number : int
  determinants_n_det
  1 : 100000000
  More than 100 million determinants

"
;;

let untouched = "
module MO_guess : sig
  type t with sexp
  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = 
  | Huckel
  | HCore
  with sexp

  let to_string = function
  | Huckel -> \"Huckel\"
  | HCore  -> \"HCore\"

  let of_string  s = 
    match (String.lowercase s) with
    | \"huckel\" -> Huckel
    | \"hcore\"  -> HCore
    | _ -> failwith (\"Wrong Guess type : \"^s)

end

module Disk_access : sig
  type t with sexp
  val to_string : t -> string
  val of_string : string -> t
end = struct
  type t = 
  | Read
  | Write
  | None
  with sexp

  let to_string = function
  | Read   -> \"Read\"
  | Write  -> \"Write\"
  | None   -> \"None\"
  let of_string  s = 
    match (String.lowercase s) with
    | \"read\"  -> Read
    | \"write\" -> Write
    | \"none\"  -> None
    | _ -> failwith (\"Wrong IO type : \"^s)

end
"

;;

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


let ezfio_template = format_of_string "
module %s : sig
  type t with sexp
  val to_%s : t -> %s
  val get_max : unit -> %s
  val of_%s : ?min:%s -> ?max:%s -> %s -> t
  val to_string : t -> string
end = struct
  type t = %s with sexp
  let to_string x = %s.to_string x
  let get_max () =
    if (Ezfio.has_%s ()) then
      Ezfio.get_%s ()
    else
      %s
  let get_min () =
      %s
  let to_%s x = x
  let of_%s ?(min=get_min ()) ?(max=get_max ()) x = 
    begin
      assert (x >= min) ;
      if (x > %s) then
        warning \"%s\";
      begin
        match max with
        | %s -> ()
        | i  -> assert ( x <= i )
      end ;
      x
    end
end
"
;;

let parse_input_ezfio input=
  let parse s = 
    match (
      String.split s ~on:'\n'
      |> List.filter ~f:(fun x -> (String.strip x) <> "")
    ) with
    | [] -> ""
    | a :: b :: c :: d :: [] ->
      begin
        let (name,typ) = String.lsplit2_exn ~on:':' a
        and ezfio_func = b
        and (min, max) = String.lsplit2_exn ~on:':' c
        and msg = d
        in 
        let (name, typ, ezfio_func, min, max, msg) = 
        match (List.map [ name ; typ ; ezfio_func ; min ; max ; msg ] ~f:String.strip) with
        | [ name ; typ ; ezfio_func ; min ; max ; msg ] -> (name, typ, ezfio_func, min, max, msg)
        | _ -> assert false
        in
        Printf.sprintf ezfio_template 
          name typ typ typ typ typ typ typ typ (String.capitalize typ)
          ezfio_func ezfio_func max min typ typ max msg min
      end
    | _ -> failwith "Error in input_ezfio"
  in
     String.split ~on:'*' input
  |> List.map ~f:parse
  |> String.concat 
  |> print_string
;;

let () = 
  parse_input input_data ;
  parse_input_ezfio input_ezfio;
  print_endline untouched;



