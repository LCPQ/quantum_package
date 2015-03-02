open Qputils;;
open Qptypes;;
open Core.Std;;

(*
 * Command-line arguments
 * ----------------------
 *)

let build_mask from upto n_int =
  let from  = MO_number.to_int from
  and upto  = MO_number.to_int upto
  and n_int = N_int_number.to_int n_int
  in
  let rec build_mask bit = function
    | 0 -> []
    | i -> 
        if ( i = upto ) then
          Bit.One::(build_mask Bit.One (i-1))
        else if ( i = from ) then
          Bit.One::(build_mask Bit.Zero (i-1))
        else
          bit::(build_mask bit (i-1))
  in
  let starting_bit = 
    if ( (upto >= n_int*64) || (upto < 0) ) then Bit.One
    else Bit.Zero
  in
  build_mask starting_bit (n_int*64)
  |> List.rev
;;


let failure s = raise (Failure s)
;;

type t = 
  | Core
  | Inactive
  | Active
  | Virtual
  | Deleted
  | None
;;

let t_to_string = function
  | Core -> "core"
  | Inactive -> "inactive"
  | Active -> "active"
  | Virtual -> "virtual"
  | Deleted -> "deleted"
  | None -> assert false
;;

let run ?(core="[]") ?(inact="[]") ?(act="[]") ?(virt="[]") ?(del="[]") ezfio_filename =

  Ezfio.set_file ezfio_filename ;
  if not (Ezfio.has_mo_basis_mo_tot_num ()) then
    failure "mo_basis/mo_tot_num not found" ;

  let mo_tot_num = Ezfio.get_mo_basis_mo_tot_num () in
  let n_int =
     try  N_int_number.of_int (Ezfio.get_determinants_n_int ())
     with _ -> Bitlist.n_int_of_mo_tot_num mo_tot_num 
  in


  let mo_class = Array.init mo_tot_num ~f:(fun i -> None) in

  (* Check input data *)
  let apply_class l = 
    let rec apply_class t = function
    | [] -> ()
    | k::tail -> let i = MO_number.to_int k in
        begin
          match mo_class.(i-1) with
          | None -> mo_class.(i-1) <- t ;
            apply_class t tail;
          | x -> failure 
             (Printf.sprintf "Orbital %d is defined both in the %s and %s spaces"
             i (t_to_string x) (t_to_string t))
        end
    in
    match l with
    | MO_class.Core     x -> apply_class Core      x
    | MO_class.Inactive x -> apply_class Inactive  x
    | MO_class.Active   x -> apply_class Active    x
    | MO_class.Virtual  x -> apply_class Virtual   x
    | MO_class.Deleted  x -> apply_class Deleted   x
  in

  let core_input = core in
  let core  = MO_class.create_core     core in
  let inact = MO_class.create_inactive inact in
  let act   = MO_class.create_active   act in
  let virt  = MO_class.create_virtual  virt in
  let del   = MO_class.create_deleted  del in

  apply_class core  ;
  apply_class inact ;
  apply_class act   ;
  apply_class virt  ;
  apply_class del   ;

  for i=1 to (Array.length mo_class)
  do
    if (mo_class.(i-1) = None) then
      failure (Printf.sprintf "Orbital %d is not specified (mo_tot_num = %d)" i mo_tot_num)
  done;
  
  
  (* Debug output *)
  MO_class.to_string core  |> print_endline ;
  MO_class.to_string inact |> print_endline ;
  MO_class.to_string act   |> print_endline ;
  MO_class.to_string virt  |> print_endline ;
  MO_class.to_string del   |> print_endline ;

  (* Create masks *)
  let ia = Excitation.create_single inact act 
  and aa = Excitation.create_single act act 
  and av = Excitation.create_single act virt
  and iv = Excitation.create_single inact virt
  in
  let single_excitations = [| ia ; aa ; av ; iv |]
    |> Array.map ~f:Excitation.(fun x ->
       match x with
       | Single (x,y) -> 
         ( MO_class.to_bitlist n_int (Hole.to_mo_class x),
           MO_class.to_bitlist n_int (Particle.to_mo_class y) ) 
       | Double _ -> assert false
       )
       
  and double_excitations = [|
    Excitation.double_of_singles ia ia ;
    Excitation.double_of_singles ia aa ;
    Excitation.double_of_singles ia iv ;
    Excitation.double_of_singles ia av ;

    Excitation.double_of_singles aa aa ;
    Excitation.double_of_singles aa iv ;
    Excitation.double_of_singles aa av ;

    Excitation.double_of_singles iv aa ;
    Excitation.double_of_singles iv av ;

(*    Excitation.double_of_singles iv iv ; *)
 |]

    |> Array.map ~f:Excitation.(fun x ->
       match x with
       | Single _ -> assert false
       | Double (x,y,z,t) -> 
         ( MO_class.to_bitlist n_int (Hole.to_mo_class x),
           MO_class.to_bitlist n_int (Particle.to_mo_class y) , 
           MO_class.to_bitlist n_int (Hole.to_mo_class z),
           MO_class.to_bitlist n_int (Particle.to_mo_class t) )
       )
  in
  
  let extract_hole (h,_) = h 
  and extract_particle (_,p) = p 
  and extract_hole1 (h,_,_,_) = h 
  and extract_particle1 (_,p,_,_) = p 
  and extract_hole2 (_,_,h,_) = h 
  and extract_particle2 (_,_,_,p) = p 
  in
(* --> TODO : This might be wrong *)
  let result_ref = 
    let core = MO_class.create_inactive core_input in
    let cv = Excitation.create_single core virt in
    let cv = match cv with
    | Excitation.Single (x,y) ->  
      ( MO_class.to_bitlist n_int (Excitation.Hole.to_mo_class x),
        MO_class.to_bitlist n_int (Excitation.Particle.to_mo_class y) ) 
    | Excitation.Double _ -> assert false
    in
    let iv = match iv with
    | Excitation.Single (x,y) ->  
      ( MO_class.to_bitlist n_int (Excitation.Hole.to_mo_class x),
        MO_class.to_bitlist n_int (Excitation.Particle.to_mo_class y) ) 
    | Excitation.Double _ -> assert false
    in
    [ Bitlist.or_operator (extract_hole iv) (extract_hole cv);
      extract_particle iv ]
  in
(* <-- TODO : This might be wrong *)

  let n_single = Array.length single_excitations in
  let n_mask = Array.length double_excitations in
  let zero = List.init (N_int_number.to_int n_int) ~f:(fun i -> 0L)
   |> Bitlist.of_int64_list in
  let result_gen = (List.init n_single ~f:(fun i-> [
    extract_hole      single_excitations.(i) ; 
    extract_particle  single_excitations.(i) ; 
    extract_hole1     double_excitations.(i) ; 
    extract_particle1 double_excitations.(i) ; 
    extract_hole2     double_excitations.(i) ; 
    extract_particle2 double_excitations.(i) ; ])
   )@(List.init (n_mask-n_single) ~f:(fun i-> [
    zero ; zero ;
    extract_hole1     double_excitations.(n_single+i) ; 
    extract_particle1 double_excitations.(n_single+i) ; 
    extract_hole2     double_excitations.(n_single+i) ; 
    extract_particle2 double_excitations.(n_single+i) ; ])
   )
   |> List.concat
  in

  (* Print bitmasks *)
  print_endline "Reference  Bitmasks:";
  List.iter ~f:(fun x-> print_endline (Bitlist.to_string x)) result_ref;

  print_endline "Generators Bitmasks:";
  List.iter ~f:(fun x-> print_endline (Bitlist.to_string x)) result_gen;

  (* Transform to int64 *)
  let result_gen =  List.map ~f:(fun x ->
     let y = Bitlist.to_int64_list x in y@y )
     result_gen 
  |> List.concat
  in
  let result_ref =  List.map ~f:(fun x ->
     let y = Bitlist.to_int64_list x in y@y )
     result_ref 
  |> List.concat
  in

  (* Write generators masks *)
  Ezfio.set_bitmasks_n_int (N_int_number.to_int n_int);
  Ezfio.set_bitmasks_bit_kind 8;
  Ezfio.set_bitmasks_n_mask_gen n_mask;
  Ezfio.ezfio_array_of_list ~rank:4 ~dim:([| (N_int_number.to_int n_int) ; 2; 6; n_mask|]) ~data:result_gen
  |> Ezfio.set_bitmasks_generators ; 

  (* Write CAS reference masks *)
  Ezfio.set_bitmasks_n_mask_cas 1;
  Ezfio.ezfio_array_of_list ~rank:3 ~dim:([| (N_int_number.to_int n_int) ; 2; 1|]) ~data:result_ref
  |> Ezfio.set_bitmasks_cas ; 


;;

let ezfio_file =
  let failure filename = 
        eprintf "'%s' is not an EZFIO file.\n%!" filename;
        exit 1
  in
  Command.Spec.Arg_type.create
  (fun filename ->
    match Sys.is_directory filename with
    | `Yes -> 
        begin
          match Sys.is_file (filename ^ "/.version") with
          | `Yes -> filename
          | _ -> failure filename
        end
    | _ -> failure filename
  )
;;

let default range =
  let failure filename = 
        eprintf "'%s' is not a regular file.\n%!" filename;
        exit 1
  in
  Command.Spec.Arg_type.create
  (fun filename ->
    match Sys.is_directory filename with
    | `Yes -> 
        begin
          match Sys.is_file (filename^"/.version") with
          | `Yes -> filename
          | _ -> failure filename
        end
    | _ -> failure filename
  )
;;

let spec =
  let open Command.Spec in
  empty 
  +> flag "core"   (optional string) ~doc:"range Range of core orbitals"
  +> flag "inact"  (optional string) ~doc:"range Range of inactive orbitals"
  +> flag "act"    (optional string) ~doc:"range Range of active orbitals"
  +> flag "virt"   (optional string) ~doc:"range Range of virtual orbitals"
  +> flag "del"    (optional string) ~doc:"range Range of deleted orbitals"
  +> anon ("ezfio_filename" %: ezfio_file)
;;

let command = 
    Command.basic 
    ~summary: "Quantum Package command"
    ~readme:(fun () ->
     "Set the orbital classes in an EZFIO directory
      The range of MOs has the form : \"[36-53,72-107,126-131]\"
        ")
    spec
    (fun core inact act virt del ezfio_filename () -> run ?core ?inact ?act ?virt ?del ezfio_filename )
;;

let () =
    Command.run command


