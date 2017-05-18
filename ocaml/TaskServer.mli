type t =
{
    queue           : Queuing_system.t ;
    state           : Message.State.t option ;
    address_tcp     : Address.Tcp.t option ;
    address_inproc  : Address.Inproc.t option ;
    psi             : Message.GetPsiReply_msg.t option;
    vector          : Message.Vector.t option ;
    progress_bar    : Progress_bar.t option ;
    running         : bool;
}


(** {1} Debugging *)

(** Fetch the QP_TASK_DEBUG environment variable *)
val debug_env : bool

(** Print a debug message *)
val debug : string -> unit

(** {1} ZMQ *)

(** ZeroMQ context *)
val zmq_context : ZMQ.Context.t

(** Bind a ZMQ socket to a TCP port and to an IPC file /tmp/qp_run.<port> *)
val bind_socket :
  socket_type:string -> socket:'a ZMQ.Socket.t -> port:int -> unit

(** Name of the host on which the server runs *)
val hostname : string lazy_t

(** IP address of the current host *)
val ip_address : string lazy_t

(** Standard messages *)
val reply_ok : [> `Req ] ZMQ.Socket.t -> unit
val reply_wrong_state : [> `Req ] ZMQ.Socket.t -> unit

(** Stop server *)
val stop : port:int -> unit

(** {1} Server functions *)

(** Create a new job *)
val new_job : Message.Newjob_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> [> `Pair] ZMQ.Socket.t -> t

(** Finish a running job *)
val end_job : Message.Endjob_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> [> `Pair] ZMQ.Socket.t -> t

(** Connect a client *)
val connect: Message.Connect_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Disconnect a client *)
val disconnect: Message.Disconnect_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Add a task to the pool *)
val add_task: Message.AddTask_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Mark the task as done by the client *)
val task_done: Message.TaskDone_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Delete a task when it has been pulled by the collector *)
val del_task: Message.DelTask_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> t

(** The client get a new task to execute *)
val get_task: Message.GetTask_msg.t -> t -> [> `Req ] ZMQ.Socket.t ->  [> `Pair] ZMQ.Socket.t -> t

(** Terminate server *)
val terminate : t -> [> `Req ] ZMQ.Socket.t -> t

(** Put a wave function in the task server *)
val put_psi :
   Message.PutPsi_msg.t -> string list -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Get the wave function stored in the task server *)
val get_psi : Message.GetPsi_msg.t -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Reply an Error message *)
val error : string -> t -> [> `Req ] ZMQ.Socket.t -> t

(** Run server *)
val run : port:int -> unit

