open Core.Std
open Qptypes

(**
The tasks server listens on a REQ socket and accepts the following commands:

* "new_job %s %s %s" state push_address_tcp push_address_inproc -> "OK"
  -> "OK"

* "connect %s" ["tcp"|"inproc"]
  -> "%d %s %s" id state push_address

* "disconnect %d" id
  -> "OK"

* "get_task %d %s" id state
  -> "%d %s" task_id task

* "task_done %d task_id %s" id state
  -> "%d %s" task_id task

*)

let bind_socket ~socket_type ~socket ~address =
  try
    ZMQ.Socket.bind socket address
  with
  | Unix.Unix_error (_, message, f) ->
    failwith @@ Printf.sprintf
        "\n%s\nUnable to bind the %s socket :\n %s\n%s"
        f socket_type address message
  | other_exception -> raise other_exception


(** Name of the host on which the server runs *)
let hostname = lazy (
    try
      Unix.gethostname ()
    with
    | _ -> "localhost"
  )


(** IP address *)
let ip_address = lazy (
  match Sys.getenv "QP_NIC" with
  | None ->
      begin
        try
          Lazy.force hostname
          |> Unix.Inet_addr.of_string_or_getbyname
          |> Unix.Inet_addr.to_string
        with
        | Unix.Unix_error _ ->
            failwith "Unable to find IP address from host name."
      end
  | Some interface ->
      begin
        try
          ok_exn Linux_ext.get_ipv4_address_for_interface interface
        with
        | Unix.Unix_error _ ->
            Lazy.force hostname
            |> Unix.Inet_addr.of_string_or_getbyname
            |> Unix.Inet_addr.to_string
      end
)


let stop ~port =
  let zmq_context =
    ZMQ.Context.create ()
  in
  let req_socket = 
    ZMQ.Socket.create zmq_context ZMQ.Socket.req
  and address =
    Printf.sprintf "tcp://%s:%d" (Lazy.force ip_address) port
  in
  ZMQ.Socket.connect req_socket address;

  Message.Terminate (Message.Terminate_msg.create ())
  |> Message.to_string
  |> ZMQ.Socket.send ~block:false req_socket ;

  let msg = 
    ZMQ.Socket.recv req_socket
    |> Message.of_string
  in
  let () =
    match msg with
    | Message.Ok _ -> ()
    | _ -> failwith "Problem in termination"
  in
  ZMQ.Socket.set_linger_period req_socket 1000;
  ZMQ.Socket.close req_socket 
  

(** Run the task server *)
let run ~port =

  let zmq_context =
    ZMQ.Context.create ()
  in

  let rep_socket = 
    ZMQ.Socket.create zmq_context ZMQ.Socket.rep
  and address =
    Printf.sprintf "tcp://%s:%d" (Lazy.force ip_address) port
  in
  bind_socket "REP" rep_socket address;

  let pollitem =
    ZMQ.Poll.mask_of
      [| (rep_socket, ZMQ.Poll.In) |]
  in

  Printf.printf "Task server running : %s\n%!" address;

  (** State variables *)
  let q = ref
     (Queuing_system.create ())
  and running = 
     ref true
  and job =
     ref None
  in

  let get_state () =
    match !job with
    | None   -> None
    | Some j -> Some j.Message.Newjob_msg.state
  in

  let get_tcp_address () =
    match !job with
    | Some j -> Address.Tcp j.Message.Newjob_msg.address_tcp
    | None   -> assert false
  in

  let get_inproc_address () =
    match !job with
    | Some j -> Address.Inproc j.Message.Newjob_msg.address_inproc
    | None   -> assert false
  in

  let ok =
    Message.Ok (Message.Ok_msg.create ())
  in

  while ( !running )
  do
    let state =
      get_state ()
    and polling =
      ZMQ.Poll.poll ~timeout:1000 pollitem
    in

    let terminate () = 
      running := false;
      Message.to_string ok
      |> ZMQ.Socket.send ~block:false rep_socket 

    and newjob x =
      q := Queuing_system.create ();
      job := Some x;
      Message.to_string ok
      |> ZMQ.Socket.send ~block:false rep_socket 

    and connect state msg = 
      let push_address = 
        match msg with
        | Message.Connect_msg.Tcp    -> get_tcp_address ()
        | Message.Connect_msg.Inproc -> get_inproc_address ()
        | Message.Connect_msg.Ipc    -> assert false
      in
      let new_q, client_id =
         Queuing_system.add_client !q
      in
      q := new_q;
      Message.ConnectReply (Message.ConnectReply_msg.create
        ~state ~client_id ~push_address)
      |> Message.to_string
      |> ZMQ.Socket.send ~block:false rep_socket

    and disconnect state msg = 
      let s, c =
        msg.Message.Disconnect_msg.state    ,
        msg.Message.Disconnect_msg.client_id
      in
      assert (s = state);
      let new_q = 
        Queuing_system.del_client ~client_id:c !q
      in
      q := new_q;
      let finished =
        Queuing_system.number_of_queued !q +
        Queuing_system.number_of_running !q = 0
      in
      Message.DisconnectReply (Message.DisconnectReply_msg.create
        ~state ~finished)
      |> Message.to_string 
      |> ZMQ.Socket.send ~block:false rep_socket

    and add_task state msg =
      let s, task =
        msg.Message.AddTask_msg.state, 
        msg.Message.AddTask_msg.task
      in
      assert (s = state);
      Message.to_string ok
      |> ZMQ.Socket.send ~block:false rep_socket
      ;
      begin
        match 
           String.split ~on:' ' msg.Message.AddTask_msg.task
           |> List.filter ~f:(fun x -> x <> "")
        with
        | "triangle" :: str_l :: [] ->
          begin
            let l =
              Int.of_string str_l
            in
            for j=1 to l
            do
              let task = 
                Printf.sprintf "%d %s" j str_l
              in
              let new_q, _ = 
                Queuing_system.add_task ~task !q
              in
              q := new_q
            done
          end
        | "range" :: str_i :: str_j :: [] ->
          begin
            let i, j =
              Int.of_string str_i,
              Int.of_string str_j
            in
            for k=i to (j+1)
            do
              let task = 
                Int.to_string k
              in
              let new_q, task_id = 
                Queuing_system.add_task ~task !q
              in
              q := new_q
            done
          end
        | _ ->
            let new_q, task_id = 
              Queuing_system.add_task ~task !q
            in
            q := new_q
      end
      
    and get_task state msg =
      let s, client_id =
        msg.Message.GetTask_msg.state, 
        msg.Message.GetTask_msg.client_id
      in
      assert (s = state);
      let new_q, task_id, task = 
        Queuing_system.pop_task ~client_id !q
      in
      q := new_q;
      let reply = 
        match (task, task_id) with
        | Some task, Some task_id ->
           Message.GetTaskReply (Message.GetTaskReply_msg.create ~task ~task_id)
        | _ -> Message.Terminate (Message.Terminate_msg.create ())
      in
      Message.to_string reply
      |> ZMQ.Socket.send ~block:false rep_socket
      
    and task_done state msg =
      let s, client_id, task_id =
        msg.Message.TaskDone_msg.state,
        msg.Message.TaskDone_msg.client_id,
        msg.Message.TaskDone_msg.task_id
      in
      assert (s = state);
      let new_q = 
        Queuing_system.end_task ~task_id ~client_id !q
      in
      q := new_q;
      Message.to_string ok
      |> ZMQ.Socket.send ~block:false rep_socket
      
    and error msg = 
      Message.Error (Message.Error_msg.create msg)
      |> Message.to_string
      |> ZMQ.Socket.send ~block:false rep_socket
    in

    if (polling.(0) = Some ZMQ.Poll.In) then
      let raw_message = 
          ZMQ.Socket.recv rep_socket
      in
      try
        let message = 
          Message.of_string raw_message
        in
(*
        Printf.printf "%d %d : %s\n%!"
        (Queuing_system.number_of_queued !q)
        (Queuing_system.number_of_running !q)
        (Message.to_string message);
          Printf.printf "%s\n%!" (Queuing_system.to_string !q);  *)
        match (state, message) with
        | _     , Message.Terminate   _ -> terminate ()
        | None  , Message.Newjob      x -> newjob x
        | None  ,                     _ -> error "No job is running"
        | _     , Message.Newjob      _ -> error "A job is already running"
        | Some s, Message.Connect     x -> connect s x
        | Some s, Message.Disconnect  x -> disconnect s x
        | Some s, Message.AddTask     x -> add_task s x
        | Some s, Message.GetTask     x -> get_task s x
        | Some s, Message.TaskDone    x -> task_done s x
        | _     , _                     ->
          error ("Invalid message : "^(Message.to_string message))
      with
      | Failure f -> error (f^" : "^raw_message)
      | Assert_failure (f,i,j) -> error (Printf.sprintf "%s:%d:%d : %s" f i j raw_message)
      
  done;   
  ZMQ.Socket.set_linger_period rep_socket 1000;
  ZMQ.Socket.close rep_socket 


(*
let () =
  Printf.printf "export QP_RUN_ADDRESS=tcp://%s:%d\n%!" (Lazy.force ip_address) (Lazy.force port)
*)


