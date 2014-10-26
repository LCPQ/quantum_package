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
;;

let keyword_to_string = function
| Ao_basis -> "AO basis"
| Bielec_integrals -> "Two electron integrals"
;;

let make_header kw =
  let s = keyword_to_string kw in
  let l = String.length s in
  "\n\n# "^s^"\n"^"# "^(String.init l ~f:(fun _ -> '='))^"\n\n"
;;

let get_bielec () = 
  (make_header Bielec_integrals)^
  (Input.Bielec_integrals.(to_string (read ())))
;;

let get_ao_basis () = 
  (make_header Ao_basis)^
  (Input.Ao_basis.(to_string (read ())))
;;

let run ezfio_filename =

  (* Open EZFIO *)
  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith (ezfio_filename^" does not exists");

  Ezfio.set_file ezfio_filename;

  let output = [
    (instructions ezfio_filename) ;
    (get_ao_basis ()) ;
    (get_bielec ()) 
  ] in
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



