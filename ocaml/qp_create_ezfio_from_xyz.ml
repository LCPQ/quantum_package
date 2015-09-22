open Qputils;;
open Qptypes;;
open Core.Std;;

let spec =
  let open Command.Spec in
  empty 
  +> flag "o" (optional string)
     ~doc:"file Name of the created EZFIO file."
  +> flag "b" (required string)
     ~doc:"string Name of basis set."
  +> flag "c" (optional_with_default 0 int)
     ~doc:"int Total charge of the molecule. Default is 0."
  +> flag "m" (optional_with_default 1 int)
     ~doc:"int Spin multiplicity (2S+1) of the molecule. Default is 1."
  +> flag "p" no_arg
     ~doc:" Using pseudopotentials"
  +> anon ("xyz_file" %: string)
;;

let run ?o b c m p xyz_file =

  (* Read molecule *)
  let molecule =
    (Molecule.of_xyz_file xyz_file ~charge:(Charge.of_int c)
      ~multiplicity:(Multiplicity.of_int m) )
  in
  let nuclei = molecule.Molecule.nuclei in

  let basis_table = Hashtbl.Poly.create () in
  (* Open basis set channels *)
  let basis_channel element =
    let key = Element.to_string element in
    match Hashtbl.find basis_table key with
    | Some in_channel -> 
        in_channel
    | None ->
        begin
         Printf.printf "%s is not defined in basis %s.\nEnter alternate basis : %!"
          (Element.to_long_string element) b ;
          let bas =
             match In_channel.input_line stdin with
             | Some line -> String.strip line |> String.lowercase
             | None -> failwith "Aborted"
          in
          let new_channel = In_channel.create 
            (Qpackage.root ^ "/data/basis/" ^ bas)
          in
          Hashtbl.add_exn basis_table ~key:key ~data:new_channel;
          new_channel
        end
  in
  
  let temp_filename =
    Filename.temp_file "qp_create_" ".basis"
  in
  let rec build_basis = function
  | [] -> ()
  | elem_and_basis_name :: rest -> 
    begin
      match (String.lsplit2 ~on:':' elem_and_basis_name) with
      | None -> (* Principal basis *)
        let basis = elem_and_basis_name in
        let command =
          if (p) then  
            Qpackage.root ^ "/scripts/get_basis.sh \"" ^ temp_filename 
              ^ "\" \"" ^ basis ^"\" pseudo"
          else
            Qpackage.root ^ "/scripts/get_basis.sh \"" ^ temp_filename 
              ^ "\" \"" ^ basis ^"\""
        in
        begin
          let filename = 
            Unix.open_process_in command
            |> In_channel.input_all
            |> String.strip
          in
          let new_channel =
            In_channel.create filename
          in
          Unix.unlink filename;
          List.iter nuclei ~f:(fun elem->
            let key = Element.to_string elem.Atom.element
            in
            match Hashtbl.add basis_table ~key:key ~data:new_channel with
            | `Ok -> ()
            | `Duplicate -> ()
          )
        end
      | Some (key, basis) -> (*Aux basis *)
        begin
          let elem  = Element.of_string key
          and basis = String.lowercase basis
          in
          let key = Element.to_string elem
          in
          let command =
              Qpackage.root ^ "/scripts/get_basis.sh \"" ^ temp_filename ^
                "\" \"" ^ basis ^ "\" " ^ key
          in
          begin
            let filename = 
              Unix.open_process_in command
              |> In_channel.input_all
              |> String.strip
            in
            let new_channel =
              In_channel.create filename
            in
            Unix.unlink filename;
            match Hashtbl.add basis_table ~key:key ~data:new_channel with
            | `Ok -> ()
            | `Duplicate -> failwith ("Duplicate definition of basis for "^(Element.to_long_string elem))
          end
       end
    end;
    build_basis rest
  in
  String.split ~on:'|' b
  |> List.rev_map ~f:String.strip 
  |> build_basis;

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

    let nmax = Nucl_number.get_max () in
    let rec do_work (accu:(Atom.t*Nucl_number.t) list) (n:int) = function
    | [] -> accu
    | e::tail ->
      let new_accu = (e,(Nucl_number.of_int ~max:nmax n))::accu in
      do_work new_accu (n+1) tail
    in
    let result = do_work [] 1  nuclei
    |> List.rev
    |> List.map ~f:(fun (x,i) ->
       try 
         Basis.read_element (basis_channel x.Atom.element) i x.Atom.element
       with
       | End_of_file -> 
         begin
           let alt_channel = basis_channel x.Atom.element in
           try
             Basis.read_element alt_channel i x.Atom.element 
           with
           End_of_file -> failwith
             ("Element "^(Element.to_string x.Atom.element)^" not found")
         end
       ) 
    |> List.concat
    in
    (* close all in_channels *)
    result
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


  (* Doesn't work... *)
  if (p) then 
    begin
      Qpackage.root ^ "/scripts/pseudo/put_pseudo_in_ezfio.py " ^ ezfio_file
      |> Sys.command_exn
    end;

  match Input.Ao_basis.read () with
  | None -> failwith "Error in basis"
  | Some x -> Input.Ao_basis.write x
 
;;

let command = 
    Command.basic 
    ~summary: "Quantum Package command"
    ~readme:(fun () -> "
Creates an EZFIO directory from a standard xyz file.
The basis set is defined as a single string if all the
atoms are taken from the same basis set, otherwise specific
elements can be defined as follows:

 -b \"cc-pcvdz | H:cc-pvdz | C:6-31g\"

      ")
    spec
    (fun o b c m p xyz_file () ->
       run ?o b c m p xyz_file )
;;

let () =
    Command.run command
;;



