open Qputils;;
open Qptypes;;
open Core.Std;;

let spec =
  let open Command.Spec in
  empty 
  +> flag "o"  (optional string)
     ~doc:"file Name of the created EZFIO file"
  +> flag "b" (required string)
     ~doc:"name Basis set."
  +> flag "c" (optional_with_default 0 int)
     ~doc:"int Total charge of the molecule. Default is 0."
  +> flag "m"  (optional_with_default 1 int)
     ~doc:"int Spin multiplicity (2S+1) of the molecule. Default is 1"
  +> anon ("xyz_file" %: string)
;;

let run ?o b c m xyz_file =

(*
  (* DEBUG *)
  Printf.printf "Charge : %d
Multiplicity : %d
Basis : %s
File : %s\n" c m b xyz_file;
*)

  (* Open basis set channel *)
  let basis_channel =
    In_channel.create
      (Qpackage.root / "data/basis" / (String.lowercase b))
  in

  (* Read molecule *)
  let molecule =
    Molecule.of_xyz_file xyz_file ~charge:c
      ~multiplicity:(Multiplicity.of_int m)
  in

  (* Build EZFIO File name *)
  let ezfio_file =
    match o with
    | Some x -> x
    | None ->
      begin
        match String.rsplit2 ~on:'.' xyz_file with
        | Some (x,"xyz") -> x^".ezfio"
        | _ -> xyz_file^".ezfio"
      end
  in
  if Sys.file_exists_exn ezfio_file then
    failwith (ezfio_file^" already exists");

  (* Create EZFIO *)
  Ezfio.set_file ezfio_file;

  (* Write Electrons *)
  Ezfio.set_electrons_elec_alpha_num ( Positive_int.to_int
    molecule.Molecule.elec_alpha ) ;
  Ezfio.set_electrons_elec_beta_num  ( Positive_int.to_int
    molecule.Molecule.elec_beta  ) ;

  (* Write Nuclei *)
  let nuclei = molecule.Molecule.nuclei in
  let labels =
     List.map ~f:(fun x->Element.to_string x.Atom.element) nuclei
  and charges = 
     List.map ~f:(fun x-> Atom.(Charge.to_float x.charge)) nuclei 
  and coords  = 
    (List.map ~f:(fun x-> x.Atom.coord.Point3d.x) nuclei) @
    (List.map ~f:(fun x-> x.Atom.coord.Point3d.y) nuclei) @
    (List.map ~f:(fun x-> x.Atom.coord.Point3d.z) nuclei) in
  let nucl_num = (List.length labels) in
  Ezfio.set_nuclei_nucl_num nucl_num ;
  Ezfio.set_nuclei_nucl_label (Ezfio.ezfio_array_of_list 
    ~rank:1 ~dim:[| nucl_num |] ~data:labels);
  Ezfio.set_nuclei_nucl_charge (Ezfio.ezfio_array_of_list 
    ~rank:1 ~dim:[| nucl_num |] ~data:charges);
  Ezfio.set_nuclei_nucl_coord  (Ezfio.ezfio_array_of_list 
    ~rank:2 ~dim:[| nucl_num ; 3 |] ~data:coords);

  (* Write Basis set *)

;;

let command = 
    Command.basic 
    ~summary: "Quantum Package command"
    ~readme:(fun () ->
      "Creates an EZFIO directory from a standard xyz file
      ")
    spec
    (fun o b c m xyz_file () ->
       run ?o b c m xyz_file )
;;

let () =
    Command.run command
;;



