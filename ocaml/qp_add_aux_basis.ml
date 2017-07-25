open Qputils
open Qptypes
open Core.Std

let spec =
  let open Command.Spec in
  empty 
  +> flag "o" (optional string)
     ~doc:"file Name of the new EZFIO file. Default is suffixed by .RI"
  +> flag "b" (required string)
     ~doc:"string Name of basis set."
  +> anon (" EZFIO file)" %: file )

type element =
| Element of Element.t
| Int_elem of (Nucl_number.t * Element.t)

    
(** Returns the list of available basis sets *)
let list_basis () =
  let basis_list = 
    Qpackage.root ^ "/install/emsl/EMSL_api.py list_basis | grep \"-RI\" " 
    |> Unix.open_process_in 
    |> In_channel.input_lines
    |> List.map ~f:(fun x -> 
       match String.split x ~on:'\'' with
       | [] -> ""
       | a :: [] 
       | _ :: a :: _ -> String.strip a
      )
  in 
  List.sort basis_list ~cmp:String.ascending
  |> String.concat ~sep:"\n" 


(** Run the program *)
let run ?o b ezfio_file =

  let new_filename = 
    match o with
    | Some filename -> filename
    | None -> ezfio_file ^ ".RI"
  in
  let () =
    match Sys.is_directory new_filename with
    | `Yes ->  failwith "Output directory exists"
    | _ -> ()
  in
  let status =
    Printf.sprintf "cp -r %s %s" ezfio_file new_filename
    |> Unix.system 
  in
  ignore status;

  Ezfio.set_file new_filename;

 (**********
  Basis set
  **********)

  let basis_table =
     Hashtbl.Poly.create ()
  in

  (* Open basis set channels *)
  let basis_channel element =
    let key =
      match element with
      | Element e -> Element.to_string e
      | Int_elem (i,e) -> Printf.sprintf "%d,%s" (Nucl_number.to_int i)  (Element.to_string e)
    in
    match Hashtbl.find basis_table key with
    | Some in_channel -> 
        in_channel
    | None -> raise Not_found
  in
  
  let temp_filename =
    Filename.temp_file "qp_create_" ".basis"
  in
  let () = 
    Sys.remove temp_filename
  in

  let fetch_channel basis =
    let command =
      Qpackage.root ^ "/scripts/get_basis.sh \"" ^ temp_filename 
          ^ "." ^ basis ^ "\" \"" ^ basis ^"\""
    in
    let long_basis = 
      Qpackage.root ^ "/data/basis/" ^ basis
    in
    match 
      Sys.is_file basis,
      Sys.is_file long_basis
    with
    | `Yes, _    -> In_channel.create basis
    | `No , `Yes -> In_channel.create long_basis
    | _ -> 
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
        new_channel
      end
  in

  let rec build_basis = function
  | [] -> ()
  | elem_and_basis_name :: rest -> 
    begin
      match (String.lsplit2 ~on:':' elem_and_basis_name) with
      | None -> (* Principal basis *)
        begin
          let basis = 
            elem_and_basis_name 
          in
          let new_channel =
            fetch_channel basis
          in
          List.iter nuclei ~f:(fun elem->
            let key = 
              Element.to_string elem.Atom.element
            in
            match Hashtbl.add basis_table ~key:key ~data:new_channel with
            | `Ok -> ()
            | `Duplicate -> ()
          )
        end
      | Some (key, basis) -> (*Aux basis *)
        begin
          let elem  = 
            try
              Element (Element.of_string key)
            with Element.ElementError _ ->
              let result = 
                match (String.split ~on:',' key) with
                | i :: k :: [] -> (Nucl_number.of_int @@ int_of_string i, Element.of_string k)
                | _ -> failwith "Expected format is int,Element:basis"
              in Int_elem result
          and basis =
            String.lowercase basis
          in
          let key = 
             match elem with
             | Element e -> Element.to_string e
             | Int_elem (i,e) -> Printf.sprintf "%d,%s" (Nucl_number.to_int i) (Element.to_string e)
          in
          let new_channel =
            fetch_channel basis
          in
          begin
            match Hashtbl.add basis_table ~key:key ~data:new_channel with
            | `Ok -> ()
            | `Duplicate ->
              let e = 
                match elem with
                | Element e -> e
                | Int_elem (_,e) -> e
              in
              failwith ("Duplicate definition of basis for "^(Element.to_long_string e))
          end
       end
    end;
    build_basis rest
  in
  String.split ~on:'|' b
  |> List.rev_map ~f:String.strip 
  |> build_basis
  in

  let basis =
    Input.AO_basis.read ()
  in
  ()


let command = 
    Command.basic 
    ~summary: "Quantum Package command"
    ~readme:(fun () -> "

=== Available basis sets ===

" ^ (list_basis ()) ^ "

============================

Extents the basis set to add an auxiliary basis set.
" )
    spec
    (fun o b ezfio_file () -> run ?o b ezfio_file )


let () =
    Command.run command




