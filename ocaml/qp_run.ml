open Core.Std;;
open Qputils;;

let print_list () =
  Lazy.force Qpackage.executables 
  |> List.iter ~f:(fun (x,_) -> Printf.printf " * %s\n" x) 
;;

let run exe ezfio_file =

  let time_start = Time.now() in

  if (not (Sys.file_exists_exn ezfio_file)) then
    failwith ("EZFIO directory "^ezfio_file^" not found");

  let executables = Lazy.force Qpackage.executables in
  if (not (List.exists ~f:(fun (x,_) -> x = exe) executables)) then
    failwith ("Executable "^exe^" not found");

  Printf.printf "===============\nQuantum Package\n===============\n\n";
  Printf.printf "Date : %s\n\n%!" (Time.to_string time_start);

  match (Sys.command ("qp_edit -c "^ezfio_file)) with
  | 0 -> ()
  | i -> failwith "Error: Input inconsistent\n";
  ;
  let exe =
    match (List.find ~f:(fun (x,_) -> x = exe) executables) with
    | None -> assert false
    | Some (_,x) -> x
  in
  match (Sys.command (exe^" "^ezfio_file)) with
  | 0 -> ()
  | i -> Printf.printf "Program exited with code %d.\n%!" i;
  ;

  let duration = Time.diff (Time.now()) time_start 
  |> Core.Span.to_string in
  Printf.printf "Wall time : %s\n\n" duration;
;;

let spec = 
  let open Command.Spec in
  empty
  +> anon ("executable" %: string)
  +> anon ("ezfio_file" %: string)
;;

let () =
  Command.basic
  ~summary: "Quantum Package command"
  ~readme:( fun () -> "
Executes a Quantum Package binary file among these:\n\n"
^ (Lazy.force Qpackage.executables
    |> List.map ~f:(fun (x,_) -> Printf.sprintf " * %s" x )
    |> String.concat ~sep:"\n" 
    )
  )
  spec
  (fun exe ezfio_file () ->
    run exe ezfio_file
  )
  |> Command.run 
;;

