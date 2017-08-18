open Core
open Qputils

(* Environment variables :

   QP_PREFIX=gdb   : to run gdb (or valgrind, or whatever)
   QP_TASK_DEBUG=1 : debug task server

*)

let print_list () =
  Lazy.force Qpackage.executables 
  |> List.iter ~f:(fun (x,_) -> Printf.printf " * %s\n" x) 

let () = 
  Random.self_init ()

let run slave exe ezfio_file =


  (** Check availability of the ports *)
  let port_number = 
    let zmq_context =
      ZMQ.Context.create ()
    in
    let dummy_socket = 
      ZMQ.Socket.create zmq_context ZMQ.Socket.rep
    in
    let rec try_new_port port_number =
      try 
        List.iter [ 0;1;2;3;4;5;6;7;8;9 ] ~f:(fun i ->
            let address = 
              Printf.sprintf "tcp://%s:%d" (Lazy.force TaskServer.ip_address) (port_number+i)
            in
            ZMQ.Socket.bind dummy_socket address;
            ZMQ.Socket.unbind dummy_socket address;
        );
        port_number
      with
      | Unix.Unix_error _ -> try_new_port (port_number+100)
    in
    let result = 
      try_new_port 41279
    in
    ZMQ.Socket.close dummy_socket;
    ZMQ.Context.terminate zmq_context;
    result
  in
  let time_start = 
    Time.now ()
  in

  if (not (Sys.file_exists_exn ezfio_file)) then
    failwith ("EZFIO directory "^ezfio_file^" not found");

  let executables = Lazy.force Qpackage.executables in
  if (not (List.exists ~f:(fun (x,_) -> x = exe) executables)) then
    begin
        Printf.printf "\nPossible choices:\n";
        List.iter executables ~f:(fun (x,_) -> Printf.printf "* %s\n%!" x);
        failwith ("Executable "^exe^" not found")
    end;

  Printf.printf "%s\n" (Time.to_string time_start);
  Printf.printf "===============\nQuantum Package\n===============\n\n";
  Printf.printf "Git Commit: %s\n" Git.message;
  Printf.printf "Git Date  : %s\n" Git.date;
  Printf.printf "Git SHA1  : %s\n" Git.sha1;
  Printf.printf "\n\n%!";


  (** Check input *)
  begin
    match (Sys.command ("qp_edit -c "^ezfio_file)) with
    | 0 -> ()
    | i -> failwith "Error: Input inconsistent\n"
  end;

  let qp_run_address_filename = 
   Filename.concat (Qpackage.ezfio_work ezfio_file) "qp_run_address"
  in

  let () = 
    if slave then
      try
        let address = 
          In_channel.read_all qp_run_address_filename
          |> String.strip
        in
        Unix.putenv ~key:"QP_RUN_ADDRESS_MASTER" ~data:address
      with Sys_error _ -> failwith "No master is not running"
  in
       
  (** Start task server *)
  let task_thread =
     let thread = 
      Thread.create ( fun () -> 
         TaskServer.run port_number )
     in
     thread ();
  in
  let address =
    Printf.sprintf "tcp://%s:%d" (Lazy.force TaskServer.ip_address) port_number
  in
  Unix.putenv ~key:"QP_RUN_ADDRESS" ~data:address;
  let () = 
    if (not slave) then
      Out_channel.with_file qp_run_address_filename  ~f:(
        fun oc -> Out_channel.output_lines oc [address]) 
  in


  (** Run executable *)
  let prefix = 
    match Sys.getenv "QP_PREFIX" with
    | Some x -> x^" "
    | None -> ""
  and exe =
    match (List.find ~f:(fun (x,_) -> x = exe) executables) with
    | Some (_,x) -> x^" "
    | None -> assert false
  in
  let exit_code = 
    match (Sys.command (prefix^exe^ezfio_file)) with
    | 0 -> 0
    | i -> (Printf.printf "Program exited with code %d.\n%!" i; i)
  in

  TaskServer.stop ~port:port_number;
  Thread.join task_thread;
  if (not slave) then
    Sys.remove qp_run_address_filename;

  let duration = Time.diff (Time.now()) time_start 
  |> Time.Span.to_string in
  Printf.printf "Wall time : %s\n\n" duration;
  if (exit_code <> 0) then
    exit exit_code

let spec = 
  let open Command.Spec in
  empty
  +> flag "slave" no_arg
     ~doc:(" Needed for slave tasks")
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
  (fun slave exe ezfio_file () ->
    run slave exe ezfio_file
  )
  |> Command.run   ~version: Git.sha1   ~build_info: Git.message


