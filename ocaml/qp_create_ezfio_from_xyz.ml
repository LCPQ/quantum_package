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

  (* Open basis set channel *)
  let basis_channel =
    In_channel.create
      (Qpackage.root ^ "/data/basis/" ^ (String.lowercase b))
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
  Ezfio.set_electrons_elec_alpha_num ( Elec_alpha_number.to_int
    molecule.Molecule.elec_alpha ) ;
  Ezfio.set_electrons_elec_beta_num  ( Elec_beta_number.to_int
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
  let basis =
    let rec do_work (accu:(Atom.t*Nucl_number.t) list) (n:int) = function
    | [] -> accu
    | e::tail -> let new_accu = (e,(Nucl_number.of_int n))::accu in
      do_work new_accu (n+1) tail
    in
    do_work [] 1  nuclei
    |> List.rev
    |> List.map ~f:(fun (x,i) ->
       Basis.read_element basis_channel i x.Atom.element) 
    |> List.concat
  in
  let long_basis = Long_basis.of_basis basis in
  let ao_num = List.length long_basis in
  Ezfio.set_ao_basis_ao_num ao_num;
  Ezfio.set_ao_basis_ao_basis b;
  let ao_prim_num = List.map long_basis ~f:(fun (_,g,_) -> List.length g.Gto.lc) 
  and ao_nucl = List.map long_basis ~f:(fun (_,_,n) -> Nucl_number.to_int n)
  and ao_power= 
    let l = List.map long_basis ~f:(fun (x,_,_) -> x) in
    (List.map l ~f:(fun t -> Positive_int.to_int Symmetry.Xyz.(t.x)) )@
    (List.map l ~f:(fun t -> Positive_int.to_int Symmetry.Xyz.(t.y)) )@
    (List.map l ~f:(fun t -> Positive_int.to_int Symmetry.Xyz.(t.z)) ) 
  in
  let ao_prim_num_max = List.fold ~init:0 ~f:(fun s x ->
    if x > s then x
    else s) ao_prim_num
  in
  let gtos = List.map long_basis ~f:(fun (_,x,_) -> x) in

  let create_expo_coef ec = 
      let coefs = 
        begin match ec with
        | `Coefs -> List.map gtos ~f:(fun x->
          List.map x.Gto.lc ~f:(fun (_,coef) -> AO_coef.to_float coef) )
        | `Expos -> List.map gtos ~f:(fun x->
          List.map x.Gto.lc ~f:(fun (prim,_) -> AO_expo.to_float
          prim.Primitive.expo) )
        end
      in
      let rec get_n n accu = function
        | [] -> List.rev accu  
        | h::tail ->
            let y =
            begin match List.nth h n with
            | Some x -> x
            | None -> 0.
            end
            in 
            get_n n (y::accu) tail
      in
      let rec build accu = function
        | n when n=ao_prim_num_max -> accu
        | n -> build ( accu @ (get_n n [] coefs) ) (n+1)
      in
      build [] 0
  in

  let ao_coef = create_expo_coef `Coefs
  and ao_expo = create_expo_coef `Expos
  in
  Ezfio.set_ao_basis_ao_prim_num (Ezfio.ezfio_array_of_list
    ~rank:1 ~dim:[| ao_num |] ~data:ao_prim_num) ;
  Ezfio.set_ao_basis_ao_nucl(Ezfio.ezfio_array_of_list
    ~rank:1 ~dim:[| ao_num |] ~data:ao_nucl) ;
  Ezfio.set_ao_basis_ao_power(Ezfio.ezfio_array_of_list
  ~rank:2 ~dim:[| ao_num ; 3 |] ~data:ao_power) ;
  Ezfio.set_ao_basis_ao_coef(Ezfio.ezfio_array_of_list
  ~rank:2 ~dim:[| ao_num ; ao_prim_num_max |] ~data:ao_coef) ;
  Ezfio.set_ao_basis_ao_expo(Ezfio.ezfio_array_of_list
  ~rank:2 ~dim:[| ao_num ; ao_prim_num_max |] ~data:ao_expo) ;
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



