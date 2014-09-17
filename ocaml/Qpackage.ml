open Core.Std;;
open Qptypes;;

(** Variables related to the quantum package installation *)

let root = 
  match (Sys.getenv "QPACKAGE_ROOT") with
  | None -> failwith "QPACKAGE_ROOT environment variable is not set.
Please source the quantum_package.rc file."
  | Some x -> x 
;;

let bit_kind_size = 
  let filename = root^"/src/Bitmask/bitmasks_module.f90" in
  if not (Sys.file_exists_exn filename) then
     raise (Failure ("File "^filename^" not found"));

  let in_channel = In_channel.create filename in
  let lines = In_channel.input_lines in_channel in
  In_channel.close in_channel;

  let rec get_data = function
  | [] -> raise (Failure ("bit_kind_size not found in "^filename)) 
  | line::tail -> 
     let line = 
     begin match String.split ~on:'!' line |> List.hd with
     | Some x -> x
     | None -> ""
     end in
     begin match (String.rsplit2 ~on:':' line) with
     | Some (_ ,buffer) -> 
       begin match (String.split ~on:'=' buffer |> List.map ~f:String.strip) with
       | ["bit_kind_size"; x] -> 
         Int.of_string x |> Bit_kind_size.of_int
       | _  -> get_data tail
       end
     | _ -> get_data tail
     end
  in
  get_data lines
;;
