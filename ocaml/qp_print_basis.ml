open Core.Std
open Qptypes

let () =
  let ezfio_filename =
    Sys.argv.(1)
  in
  if (not (Sys.file_exists_exn ezfio_filename)) then
    failwith "Error reading EZFIO file";
  Ezfio.set_file ezfio_filename;
  let basis =
     match Input.Ao_basis.read () with
    | Some basis -> basis
    | _ -> failwith "Error reading basis set"
  in
  Input.Ao_basis.to_rst basis
  |> Rst_string.to_string 
  |> print_endline 
  

