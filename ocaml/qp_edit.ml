open Qputils;;
open Qptypes;;
open Core.Std;;

let file_header filename = Printf.sprintf
"
==================================================================
                       Quantum Package
==================================================================

Editing file `%s`

" filename
  
type keyword = 
| Ao_basis
| Bielec_integrals
| Cisd_sc2
| Determinants
| Electrons
| Full_ci
| Hartree_fock
| Mo_basis
| Nuclei
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
| Nuclei           -> "Molecule"
;;

let make_header kw =
  let s = keyword_to_string kw in
  let l = String.length s in
  "\n\n"^s^"\n"^(String.init l ~f:(fun _ -> '='))^"\n\n"
;;

let get s = 
  let header = (make_header s) 
  and rst = match s with
  | Full_ci ->
    Input.Full_ci.(to_rst (read ()))
  | Hartree_fock ->
    Input.Hartree_fock.(to_rst (read ()))
  | Mo_basis ->
    Input.Mo_basis.(to_rst (read ()))
  | Electrons ->
    Input.Electrons.(to_rst (read ()))
  | Determinants ->
    Input.Determinants.(to_rst (read ()))
  | Cisd_sc2 ->
    Input.Cisd_sc2.(to_rst (read ()))
  | Nuclei ->
    Input.Nuclei.(to_rst (read ()))
  | Ao_basis ->
    Input.Ao_basis.(to_rst (read ()))
  | Bielec_integrals -> 
    Input.Bielec_integrals.(to_rst (read ()))
 
  in header^(Rst_string.to_string rst)
;;

let set str s = 
  let header = (make_header s) in
  let index_begin = String.substr_index_exn ~pos:0 ~pattern:header str in
  let index_begin = index_begin + (String.length header) in
  let index_end   = 
    match ( String.substr_index ~pos:(index_begin+(String.length header)+1)
      ~pattern:"==" str) with
      | Some i -> i
      | None -> String.length str
  in
  let l = index_end - index_begin in
  let str = String.sub ~pos:index_begin ~len:l str
  |> Rst_string.of_string
  in
  match s with
  (*
  | Full_ci ->
  | Hartree_fock ->
  | Mo_basis ->
    *)
  | Electrons ->
      Input.Electrons.of_rst str 
      |> Input.Electrons.write
  | Determinants ->
      Input.Determinants.of_rst str 
      |> Input.Determinants.write
  | Cisd_sc2 ->
      Input.Cisd_sc2.of_rst str 
      |> Input.Cisd_sc2.write
  | Nuclei ->
      Input.Nuclei.of_rst str 
      |> Input.Nuclei.write
  | Bielec_integrals -> 
      Input.Bielec_integrals.of_rst str 
      |> Input.Bielec_integrals.write
    (*
  | Ao_basis ->
    *)
 
;;


let create_temp_file ezfio_filename fields =
  let temp_filename  = Filename.temp_file "qp_edit_" ".rst" in
  Out_channel.with_file temp_filename ~f:(fun out_channel ->
    (file_header ezfio_filename) :: (List.map ~f:get fields) 
    |> String.concat ~sep:"\n" 
    |> Out_channel.output_string out_channel 
  );
  temp_filename
;;

let run ezfio_filename =

  (* Open EZFIO *)
  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith (ezfio_filename^" does not exists");

  Ezfio.set_file ezfio_filename;

  (*
  let output = (file_header ezfio_filename) :: (
    List.map ~f:get [
      Ao_basis ; 
      Mo_basis ; 
      Full_ci ;
      Hartree_fock ;
    ])
   in
  String.concat output
  |> print_string
  *)
  
  let tasks = [
      Nuclei ;
      Electrons ;
      Bielec_integrals ;
      Cisd_sc2 ;
      Determinants ;
  ]
  in

  (* Create the temp file *)
  let temp_filename = create_temp_file ezfio_filename tasks in

  (* Open the temp file with external editor *)
  let editor = 
    match Sys.getenv "EDITOR" with
    | Some editor -> editor
    | None -> "vi"
  in
  let command = Printf.sprintf "%s %s" editor temp_filename in
  Sys.command_exn command;

  (* Re-read the temp file *)
  let temp_string  =
    In_channel.with_file temp_filename ~f:(fun in_channel ->
      In_channel.input_all in_channel) 
  in
  List.iter ~f:(fun x -> set temp_string x) tasks;

  (* Remove temp_file *)
  Sys.remove temp_filename;
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



