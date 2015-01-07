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
  let header = (make_header s) in
  let f (read,to_rst) = 
    match read () with
    | Some text -> header ^ (Rst_string.to_string (to_rst text))
    | None      -> ""
  in
  let rst = 
    try
      begin
         let open Input in
         match s with
         | Full_ci ->
           f Full_ci.(read, to_rst)
         | Hartree_fock ->
           f Hartree_fock.(read, to_rst)
         | Mo_basis ->
           f Mo_basis.(read, to_rst)
         | Electrons ->
           f Electrons.(read, to_rst)
         | Cisd_sc2 ->
           f Cisd_sc2.(read, to_rst)
         | Nuclei ->
           f Nuclei.(read, to_rst)
         | Ao_basis ->
           f Ao_basis.(read, to_rst)
         | Bielec_integrals -> 
           f Bielec_integrals.(read, to_rst)
         | Determinants ->
           f Determinants.(read, to_rst)
      end
    with
    | Sys_error msg -> (Printf.eprintf "Info: %s\n%!" msg ; "")
  in 
  rst
;;

let set str s = 
  let header = (make_header s) in
  match String.substr_index ~pos:0 ~pattern:header str with
  | None -> ()
  | Some idx -> 
    begin
      let index_begin = idx + (String.length header) in
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
      let write (of_rst,w) s =
        try
        match of_rst str with
        | Some data -> w data
        | None -> ()
        with
        | _ -> (Printf.eprintf "Info: Read error in %s\n%!"
               (keyword_to_string s))
      in
      let open Input in
        match s with
        | Hartree_fock     -> write Hartree_fock.(of_rst, write) s
        | Full_ci          -> write Full_ci.(of_rst, write) s
        | Electrons        -> write Electrons.(of_rst, write) s
        | Cisd_sc2         -> write Cisd_sc2.(of_rst, write) s
        | Bielec_integrals -> write Bielec_integrals.(of_rst, write) s
        | Determinants     -> write Determinants.(of_rst, write) s
        | Nuclei           -> write Nuclei.(of_rst, write) s
        | Ao_basis         -> () (* TODO *)
        | Mo_basis         -> () (* TODO *)
    end 
;;


let create_temp_file ezfio_filename fields =
  let temp_filename  = Filename.temp_file "qp_edit_" ".rst" in
  begin
      Out_channel.with_file temp_filename ~f:(fun out_channel ->
        (file_header ezfio_filename) :: (List.map ~f:get fields) 
        |> String.concat ~sep:"\n" 
        |> Out_channel.output_string out_channel 
      )
  end
  ; temp_filename
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
    ])
   in
  String.concat output
  |> print_string
  *)
  
  let tasks = [
      Nuclei ;
      Ao_basis;
      Electrons ;
      Bielec_integrals ;
      Hartree_fock ;
      Cisd_sc2 ;
      Full_ci ;
      Mo_basis;
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



