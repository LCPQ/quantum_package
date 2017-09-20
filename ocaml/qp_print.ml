open Qputils;;
open Qptypes;;
open Core;;

type input_action =
  | Basis
  | Nuclei
  | Charge
  | Multiplicity
  | Electrons
;;

let create_i_action = function
  | "basis" -> Basis
  | "nucl" -> Nuclei
  | "charge" -> Charge
  | "mult" -> Multiplicity
  | "elec" -> Electrons
  | _ -> raise (Failure "Action should be:
  * basis
  * nucl
  * charge
  * mult
  * elec
")

;;

let spec =
  let open Command.Spec in
  empty 
  +> flag "i"  (optional string)
     ~doc:"Prints input data"
  +> flag "o" (optional string)
     ~doc:"Prints output data"
  +> anon ("ezfio_file" %: string)
;;

let run_i ~action ezfio_filename =

  let action = create_i_action action in

  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith (ezfio_filename^" does not exist");

  Ezfio.set_file ezfio_filename;

  let print_basis () =
    Printf.printf "%s\n" (Ezfio.get_ao_basis_ao_basis ())
  in


  let compute_charge () =
    let input = match Input.Electrons.read () with
    | Some x -> x
    | None   -> assert false
    in
    let nucl_charge = Ezfio.get_nuclei_nucl_charge ()
      |> Ezfio.flattened_ezfio |> Array.map ~f:(Float.to_int) 
    and n_alpha = input.Input.Electrons.elec_alpha_num
      |> Elec_alpha_number.to_int
    and n_beta  = input.Input.Electrons.elec_beta_num 
      |> Elec_beta_number.to_int
    in Array.fold ~init:(-n_alpha-n_beta) ~f:(fun x y -> x+y) nucl_charge
    |> Charge.of_int
  in

  let compute_multiplicity () =
    let input = match Input.Electrons.read () with
    | Some x -> x
    | None   -> assert false
    in
    let n_alpha = input.Input.Electrons.elec_alpha_num
    and n_beta  = input.Input.Electrons.elec_beta_num 
    in Multiplicity.of_alpha_beta n_alpha n_beta
  in

  let create_molecule () =
    let nucl_num = Ezfio.get_nuclei_nucl_num () 
    and nucl_charge = Ezfio.get_nuclei_nucl_charge ()
       |> Ezfio.flattened_ezfio
    and nucl_coord = Ezfio.get_nuclei_nucl_coord ()
       |> Ezfio.flattened_ezfio
    in
    let nucl_label =
      if (Ezfio.has_nuclei_nucl_label ()) then
       Ezfio.get_nuclei_nucl_label () |> Ezfio.flattened_ezfio
      else
        Array.map ~f:(fun x-> x
          |> Charge.of_float
          |> Element.of_charge 
          |> Element.to_string ) nucl_charge
    in
    let buffer = ref "" in
    for i=0 to (nucl_num-1) do
      buffer := !buffer ^ (Printf.sprintf "%s %f %f %f %f\n"
        nucl_label.(i)
        nucl_charge.(i)
        nucl_coord.(i)
        nucl_coord.(i+nucl_num)
        nucl_coord.(i+nucl_num+nucl_num)
      )
    done ;
    let charge = compute_charge () in
    let mult = compute_multiplicity () in
    Molecule.of_xyz_string ~charge:charge ~multiplicity:mult !buffer
  in

  let print_nuclei () =
    let molecule = create_molecule () in
    print_endline (Molecule.to_string molecule)

  and print_charge () = 
    let molecule = create_molecule () in
    Printf.printf "%s" (Charge.to_string (Molecule.get_charge molecule))

  and print_multiplicity () = 
    let molecule = create_molecule () in
    Printf.printf "%s" (Multiplicity.to_string (Molecule.get_multiplicity
    molecule))

  and print_electrons () = ()

  in
  match action with
  | Basis        -> print_basis ()
  | Nuclei       -> print_nuclei ()
  | Charge       -> print_charge ()
  | Multiplicity -> print_multiplicity ()
  | Electrons    -> print_electrons ()
;;

let run_o ~action ezfio_filename =

  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith (ezfio_filename^" does not exist");

  (* Open EZFIO *)
  Ezfio.set_file ezfio_filename;

;;

let command = 
    Command.basic 
    ~summary: "Quantum Package command"
    ~readme:(fun () ->
      "
Prints data contained into the EZFIO file.

Input data :

  * basis
  * nucl
  * charge
  * mult
  * elec

Output data :

  * 
      ")
    spec
    (fun i o ezfio_file () ->
  try 
      match (i,o) with
      | (Some i, None) -> run_i ~action:i ezfio_file
      | (None, Some o) -> run_o ~action:o ezfio_file
      | (Some _, Some _) -> 
          raise (Failure "Error : please specify -i or -o but not both.")
      | (None, None) -> 
          raise (Failure "Error : please specify -i or -o.")
  with
  | Failure msg -> print_string ("Error\n"^msg)
  | _ -> ()
    )
;;

let () =
    Command.run command
;;



