open Core.Std
open Qptypes

let molecule = lazy (
    let atom_list = 
        match Input.Nuclei.read () with
        | Some data ->  Input.Nuclei.to_atom_list data
        | None -> failwith "No coordinate found"
    and data = 
        match Input.Electrons.read () with
        | Some data -> data
        | None -> failwith "No electrons found"
    in
    { Molecule.nuclei = atom_list;
      Molecule.elec_alpha = data.Input.Electrons.elec_alpha_num;
      Molecule.elec_beta = data.Input.Electrons.elec_beta_num
    }
)

let write_xyz_file libint_dir =
    assert (Sys.is_directory_exn libint_dir);
    let filename = 
       Filename.concat libint_dir "xyz" 
    and molecule = 
        Lazy.force molecule
    in
    Out_channel.with_file  filename ~f:(fun oc -> 
        Molecule.to_xyz molecule
        |> Out_channel.output_string oc
    )

let write_basis_file libint_dir = 
    assert (Sys.is_directory_exn libint_dir);
    let filename = 
       Filename.concat libint_dir "basis.g94" 
    and molecule = 
        Lazy.force molecule
    in
    let text = 
       let rec substitute accu i = function
       | ele :: tail -> 
         let pattern = 
           Printf.sprintf "Atom %d" i
         in
         let new_string = 
            String.substr_replace_first accu ~pattern ~with_:(Printf.sprintf "%s  0" ele)
         in
         substitute new_string (i+1) tail
       | [] -> accu
       in
       let accu = 
          let b = 
            match Input.Ao_basis.read () with
            | Some data -> Input.Ao_basis.to_basis data
            | None -> failwith "No AO basis"
          in
          Basis.to_string ~fmt:Gto.Gaussian b
       and atom_names =
          List.map molecule.Molecule.nuclei ~f:(fun x -> Element.to_string x.Atom.element)
       in
       substitute accu 1 atom_names
    in
    Out_channel.with_file  filename ~f:(fun oc -> 
        Out_channel.output_string oc text
    )
    

let write_files ezfio_filename =
    assert (Sys.is_directory_exn ezfio_filename);

    let libint_dir =
      Filename.concat ezfio_filename "libint" 
    in

    let () =
      match Sys.is_directory libint_dir with
      | `Yes -> ()
      | `No -> Unix.mkdir libint_dir
      | `Unknown -> failwith ("Unable to tell if "^libint_dir^" exists.")
    in

    write_xyz_file libint_dir;
    write_basis_file libint_dir


