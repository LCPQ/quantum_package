open Qputils;;
open Qptypes;;
open Core.Std;;

let instructions filename = Printf.sprintf
"# ===============
# Quantum Package
# ===============
# 
# File : %s
#
# Lines starting with a '#' sign are commented.
#" filename
  
type keyword = 
| Ao_basis
| Bielec_integrals
| Cisd_sc2
| Determinants
| Electrons
| Full_ci
| Hartree_fock
| Mo_basis
;;

let keyword_to_string = function
| Ao_basis         -> "AO basis"
| Bielec_integrals -> "Two electron integrals"
| Cisd_sc2         -> "CISD (SC)^2"
| Determinants     -> "Determinants"
| Electrons        -> "Electrons"
| Full_ci          -> "Selected Full-CI"
| Hartree_fock     -> "Hartree-Fock"
| Mo_basis         -> "MO basis"
;;

let make_header kw =
  let s = keyword_to_string kw in
  let l = String.length s in
  "\n\n# "^s^"\n"^"# "^(String.init l ~f:(fun _ -> '='))^"\n\n"
;;

let get s = 
  let header = (make_header s)
  in header^(match s with
  | Bielec_integrals -> 
    Input.Bielec_integrals.(to_string (read ()))
  | Ao_basis ->
    Input.Ao_basis.(to_string (read ()))
  | Cisd_sc2 ->
    Input.Cisd_sc2.(to_string (read ()))
  | Determinants ->
    Input.Determinants.(to_string (read ()))
  | Electrons ->
    Input.Electrons.(to_string (read ()))
  | Full_ci ->
    Input.Full_ci.(to_string (read ()))
  | Hartree_fock ->
    Input.Hartree_fock.(to_string (read ()))
  | Mo_basis ->
    Input.Mo_basis.(to_string (read ()))
  )
;;

(*
let create_temp_file ezfio_filename fields =
  In_channel.with_file filename ~f:(func out_channel ->
    (instructions ezfio_filename) :: (List.map ~f:get fields)
    |> String.concat output
    |> print_string
  )
;;
*)

let run ezfio_filename =

  (* Open EZFIO *)
  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith (ezfio_filename^" does not exists");

  Ezfio.set_file ezfio_filename;

  let output = (instructions ezfio_filename) :: (
    List.map ~f:get [
      Electrons ;
      Ao_basis ; 
      Mo_basis ; 
      Bielec_integrals ;
      Cisd_sc2 ;
      Determinants ;
      Full_ci ;
      Hartree_fock ;
    ])
   in
  String.concat output
  |> print_string
;;



let spec =
  let open Command.Spec in
  empty 
(*
  +> flag "i"  (optional string)
     ~doc:"Prints input data"
  +> flag "o" (optional string)
     ~doc:"Prints output data"
*)
  +> anon ("ezfio_file" %: string)
;;

let command = 
    Command.basic 
    ~summary: "Quantum Package command"
    ~readme:(fun () ->
      "
Edit input data
      ")
    spec
(*    (fun i o ezfio_file () -> *)
    (*fun ezfio_file () -> 
       try 
           run ezfio_file
       with
       | _ msg -> print_string ("\n\nError\n\n"^msg^"\n\n")
    *)
    (fun ezfio_file () -> run ezfio_file)
;;

let () =
    Command.run command
;;



