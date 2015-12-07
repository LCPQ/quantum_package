open Core.Std

(** New job : Request to create a new multi-tasked job *)

module State : sig
  type t 
  val of_string : string -> t
  val to_string : t -> string 
end = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

module Newjob_msg : sig
  type t = 
  { state: State.t;
    address_tcp: Address.Tcp.t ;
    address_inproc: Address.Inproc.t;
  }
  val create : address_tcp:string -> address_inproc:string -> state:string -> t
  val to_string : t -> string
end = struct
  type t = 
  { state: State.t;
    address_tcp: Address.Tcp.t ;
    address_inproc: Address.Inproc.t;
  }
  let create ~address_tcp ~address_inproc ~state =
    { state = State.of_string state;
      address_tcp = Address.Tcp.of_string address_tcp ;
      address_inproc = Address.Inproc.of_string address_inproc ;
    }
  let to_string t =
    Printf.sprintf "newjob %s %s %s"
     ( State.to_string t.state ) 
     ( Address.Tcp.to_string t.address_tcp ) 
     ( Address.Inproc.to_string t.address_inproc ) 
end


(** Connect : connect a new client to the task server *)

module Connect_msg : sig
  type t = Tcp | Inproc | Ipc
  val create : typ:string -> t
  val to_string : t -> string
end = struct
  type t = Tcp | Inproc | Ipc
  let create ~typ = 
    match typ with
    | "tcp" -> Tcp
    | "inproc" -> Inproc
    | "ipc" -> Ipc
    |  _ -> assert false
  let to_string = function
    | Tcp    -> "connect tcp"
    | Inproc -> "connect inproc"
    | Ipc    -> "connect ipc"
end

(** ConnectReply : Reply to the connect messsage *)

module ConnectReply_msg : sig
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
    push_address: Address.t;
  }
  val create : state:State.t -> client_id:Id.Client.t -> push_address:Address.t -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
    push_address: Address.t;
  }
  let create ~state ~client_id ~push_address = 
    { client_id ; state ; push_address }
  let to_string x =
    Printf.sprintf "connect_reply %s %d %s"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
      (Address.to_string x.push_address)
end


(** Disconnect : disconnect a client from the task server *)
module Disconnect_msg : sig
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  val create : state:string -> client_id:string -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  let create ~state ~client_id = 
    { client_id = Id.Client.of_string client_id ; state = State.of_string state }
  let to_string x =
    Printf.sprintf "disconnect %s %d"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
end

module DisconnectReply_msg : sig
  type t = 
  { finished: bool ;
    state: State.t ;
  }
  val create : state:State.t -> finished:bool -> t
  val to_string : t -> string
end = struct
  type t = 
  { finished: bool;
    state: State.t ;
  }
  let create ~state ~finished = 
    { state ; finished }
  let to_string x =
    Printf.sprintf "disconnect_reply %s %d"
      (State.to_string x.state)
      (if x.finished then 1 else 0)
end



(** AddTask : Add a new task to the queue *)
module AddTask_msg : sig
  type t = 
  { state: State.t;
    task:  string;
  }
  val create : state:string -> task:string -> t
  val to_string : t -> string
end = struct
  type t = 
  { state: State.t;
    task:  string;
  }
  let create ~state ~task = { state = State.of_string state ; task }
  let to_string x =
    Printf.sprintf "add_task %s %s" (State.to_string x.state) x.task 
end


(** AddTaskReply : Reply to the AddTask message *)
module AddTaskReply_msg : sig
  type t  
  val create : task_id:Id.Task.t -> t
  val to_string : t -> string
end = struct
  type t = Id.Task.t
  let create ~task_id = task_id
  let to_string x =
    Printf.sprintf "add_task_reply %d" (Id.Task.to_int x)
end


(** GetTask : get a new task to do *)
module GetTask_msg : sig
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  val create : state:string -> client_id:string -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  let create ~state ~client_id = 
    { client_id = Id.Client.of_string client_id ; state = State.of_string state }
  let to_string x =
    Printf.sprintf "get_task %s %d"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
end

(** GetTaskReply : Reply to the GetTask message *)
module GetTaskReply_msg : sig
  type t  
  val create : task_id:Id.Task.t -> task:string -> t
  val to_string : t -> string
end = struct
  type t =
  { task_id: Id.Task.t ;
    task   : string ;
  }
  let create ~task_id ~task = { task_id ; task }
  let to_string x =
    Printf.sprintf "get_task_reply %d %s" (Id.Task.to_int x.task_id) x.task
end


(** TaskDone : Inform the server that a task is finished *)
module TaskDone_msg : sig
  type t =
  { client_id: Id.Client.t ;
    state: State.t ;
    task_id:  Id.Task.t;
  }
  val create : state:string -> client_id:string -> task_id:string -> t
  val to_string : t -> string
end = struct
  type t =
  { client_id: Id.Client.t ;
    state: State.t ;
    task_id:  Id.Task.t;
  }
  let create ~state ~client_id ~task_id = 
    { client_id = Id.Client.of_string client_id ; 
      state = State.of_string state ;
      task_id  = Id.Task.of_string task_id }
  let to_string x =
    Printf.sprintf "task_done %s %d %d"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
      (Id.Task.to_int x.task_id)
end

(** Terminate *)
module Terminate_msg : sig
  type t 
  val create : unit -> t
  val to_string : t -> string
end = struct
  type t = Terminate
  let create () = Terminate
  let to_string x = "terminate"
end

(** OK *)
module Ok_msg : sig
  type t 
  val create : unit -> t
  val to_string : t -> string
end = struct
  type t = Ok
  let create () = Ok
  let to_string x = "ok"
end

(** Error *)
module Error_msg : sig
  type t 
  val create : string -> t
  val to_string : t -> string
end = struct
  type t = string
  let create x = x
  let to_string x =
     String.concat ~sep:" "  [ "error" ; x ] 
end



(** Message *)

type t =
| Newjob          of Newjob_msg.t
| Connect         of Connect_msg.t
| ConnectReply    of ConnectReply_msg.t
| Disconnect      of Disconnect_msg.t
| DisconnectReply of DisconnectReply_msg.t
| GetTask         of GetTask_msg.t
| GetTaskReply    of GetTaskReply_msg.t
| AddTask         of AddTask_msg.t
| AddTaskReply    of AddTaskReply_msg.t
| TaskDone        of TaskDone_msg.t
| Terminate       of Terminate_msg.t
| Ok              of Ok_msg.t
| Error           of Error_msg.t


let of_string s = 
  let l =
    String.split ~on:' ' s
    |> List.map ~f:String.strip
    |> List.map ~f:String.lowercase
    |> List.filter ~f:(fun x -> (String.strip x) <> "")
  in
  match l with
  | "add_task"   :: state :: task ->
       AddTask (AddTask_msg.create ~state ~task:(String.concat ~sep:" " task) )
  | "get_task"   :: state :: client_id :: [] ->
       GetTask (GetTask_msg.create ~state ~client_id)
  | "task_done"  :: state :: client_id :: task_id :: [] ->
       TaskDone (TaskDone_msg.create ~state ~client_id ~task_id)
  | "disconnect" :: state :: client_id :: [] ->
       Disconnect (Disconnect_msg.create ~state ~client_id)
  | "connect"    :: t :: [] -> 
       Connect (Connect_msg.create t)
  | "new_job"    :: state :: push_address_tcp :: push_address_inproc :: [] -> 
       Newjob (Newjob_msg.create push_address_tcp push_address_inproc state)
  | "terminate"  :: [] ->
       Terminate (Terminate_msg.create () )
  | "ok"         :: [] ->
       Ok (Ok_msg.create ())
  | "error"      :: rest ->
       Error (Error_msg.create (String.concat ~sep:" " rest))
  | _ -> failwith "Message not understood"
    

let to_string = function
| Newjob        x -> Newjob_msg.to_string       x
| Connect       x -> Connect_msg.to_string      x
| ConnectReply  x -> ConnectReply_msg.to_string x
| Disconnect    x -> Disconnect_msg.to_string   x
| DisconnectReply  x -> DisconnectReply_msg.to_string x
| GetTask       x -> GetTask_msg.to_string      x
| GetTaskReply  x -> GetTaskReply_msg.to_string x
| AddTask       x -> AddTask_msg.to_string      x
| AddTaskReply  x -> AddTaskReply_msg.to_string x
| TaskDone      x -> TaskDone_msg.to_string     x
| Terminate     x -> Terminate_msg.to_string    x
| Ok            x -> Ok_msg.to_string           x
| Error         x -> Error_msg.to_string        x


