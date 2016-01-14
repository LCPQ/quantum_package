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

  Printf.printf "%s\n" (Time.to_string time_start);
  Printf.printf "===============\nQuantum Package\n===============\n\n";
  Printf.printf "Git Commit: %s\n" Git.message;
  Printf.printf "Git Date  : %s\n" Git.date;
  Printf.printf "Git SHA1  : %s\n" Git.sha1;
  Printf.printf "\n\n%!";


  (** Check input *)
  match (Sys.command ("qp_edit -c "^ezfio_file)) with
  | 0 -> ()
  | i -> failwith "Error: Input inconsistent\n";
  ;


  (** Start task server *)
  let port_number = 
    12345
  in
  let address =
    Printf.sprintf "tcp://%s:%d" (Lazy.force TaskServer.ip_address) port_number
  in
  let task_thread =
     let thread = 
      Thread.create ( fun () -> 
         TaskServer.run port_number )
     in
     thread ();
  in
  Unix.putenv ~key:"QP_RUN_ADDRESS" ~data:address;

  (** Run executable *)
  let exe =
    match (List.find ~f:(fun (x,_) -> x = exe) executables) with
    | None -> assert false
    | Some (_,x) -> x
  in
  match (Sys.command (exe^" "^ezfio_file)) with
  | 0 -> ()
  | i -> Printf.printf "Program exited with code %d.\n%!" i;
  ;

  TaskServer.stop ~port:port_number;
  Thread.join task_thread;

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
  |> Command.run   ~version: Git.sha1   ~build_info: Git.message
;;


