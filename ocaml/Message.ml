open Core
open Qptypes

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
    Printf.sprintf "new_job %s %s %s"
     ( State.to_string t.state ) 
     ( Address.Tcp.to_string t.address_tcp ) 
     ( Address.Inproc.to_string t.address_inproc ) 
end

module Endjob_msg : sig
  type t = 
  { state: State.t;
  }
  val create : state:string -> t
  val to_string : t -> string
end = struct
  type t = 
  { state: State.t;
  }
  let create ~state =
    { state = State.of_string state;
    }
  let to_string t =
    Printf.sprintf "end_job %s"
     ( State.to_string t.state ) 
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
  val create : state:string -> client_id:int -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  let create ~state ~client_id = 
    { client_id = Id.Client.of_int client_id ; state = State.of_string state }
  let to_string x =
    Printf.sprintf "disconnect %s %d"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
end

module DisconnectReply_msg : sig
  type t = 
  { 
    state: State.t ;
  }
  val create : state:State.t -> t
  val to_string : t -> string
end = struct
  type t = 
  { 
    state: State.t ;
  }
  let create ~state = 
    { state }
  let to_string x =
    Printf.sprintf "disconnect_reply %s"
      (State.to_string x.state)
end



(** AddTask : Add a new task to the queue *)
module AddTask_msg : sig
  type t = 
  { state: State.t;
    tasks:  string list;
  }
  val create : state:string -> tasks:string list -> t
  val to_string : t -> string
end = struct
  type t = 
  { state: State.t;
    tasks:  string list;
  }
  let create ~state ~tasks = { state = State.of_string state ; tasks }
  let to_string x =
    Printf.sprintf "add_task %s %s" (State.to_string x.state) (String.concat ~sep:"|" x.tasks)
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


(** DelTask : Remove a task from the queue *)
module DelTask_msg : sig
  type t = 
  { state:  State.t;
    task_ids:  Id.Task.t list
  }
  val create : state:string -> task_ids:int list -> t
  val to_string : t -> string
end = struct
  type t = 
  { state:  State.t;
    task_ids:  Id.Task.t list
  }
  let create ~state ~task_ids =
    { state = State.of_string state ; 
      task_ids = List.map ~f:Id.Task.of_int task_ids
    }
  let to_string x =
    Printf.sprintf "del_task %s %s" 
      (State.to_string x.state)
      (String.concat ~sep:"|" @@ List.map ~f:Id.Task.to_string x.task_ids)
end


(** DelTaskReply : Reply to the DelTask message *)
module DelTaskReply_msg : sig
  type t  
  val create : task_ids:Id.Task.t list -> more:bool -> t
  val to_string : t -> string
end = struct
  type t = {
    task_ids : Id.Task.t list;
    more    : bool;
  }
  let create ~task_ids ~more = { task_ids ; more }
  let to_string x =
    let more = 
      if x.more then "more"
      else "done"
    in
    Printf.sprintf "del_task_reply %s %s" 
     more (String.concat ~sep:"|" @@ List.map ~f:Id.Task.to_string x.task_ids) 
end



(** GetTask : get a new task to do *)
module GetTask_msg : sig
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  val create : state:string -> client_id:int -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
    state: State.t ;
  }
  let create ~state ~client_id = 
    { client_id = Id.Client.of_int client_id ; state = State.of_string state }
  let to_string x =
    Printf.sprintf "get_task %s %d"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
end

(** GetTaskReply : Reply to the GetTask message *)
module GetTaskReply_msg : sig
  type t  
  val create : task_id:Id.Task.t option -> task:string option -> t
  val to_string : t -> string
end = struct
  type t =
  { task_id: Id.Task.t option ;
    task   : string option ;
  }
  let create ~task_id ~task = { task_id ; task }
  let to_string x =
    match x.task_id, x.task with
    | Some task_id, Some task -> 
      Printf.sprintf "get_task_reply %d %s" (Id.Task.to_int task_id) task
    | _ ->
      Printf.sprintf "get_task_reply 0"
end

(** GetPsi : get the current variational wave function *)
module GetPsi_msg : sig
  type t = 
  { client_id: Id.Client.t ;
  }
  val create : client_id:int -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
  }
  let create ~client_id = 
    { client_id = Id.Client.of_int client_id }
  let to_string x =
    Printf.sprintf "get_psi %d"
      (Id.Client.to_int x.client_id)
end

module Psi : sig
  type t = 
  {
      n_state   :  Strictly_positive_int.t   ;
      n_det     :  Strictly_positive_int.t   ;
      psi_det_size :  Strictly_positive_int.t ;
      n_det_generators : Strictly_positive_int.t option;
      n_det_selectors : Strictly_positive_int.t option;
      psi_det   :  string                    ;
      psi_coef  :  string                    ;
      energy    :  string;
  }
  val create : n_state:Strictly_positive_int.t
     -> n_det:Strictly_positive_int.t 
     -> psi_det_size:Strictly_positive_int.t 
     -> n_det_generators:Strictly_positive_int.t option
     -> n_det_selectors:Strictly_positive_int.t option
     -> psi_det:string -> psi_coef:string -> energy:string -> t
end = struct
  type t = 
  {
      n_state   :  Strictly_positive_int.t   ;
      n_det     :  Strictly_positive_int.t   ;
      psi_det_size :  Strictly_positive_int.t ;
      n_det_generators : Strictly_positive_int.t option;
      n_det_selectors : Strictly_positive_int.t option;
      psi_det   :  string                    ;
      psi_coef  :  string                    ;
      energy    :  string                    ;
  }
  let create ~n_state ~n_det ~psi_det_size
    ~n_det_generators ~n_det_selectors ~psi_det ~psi_coef 
    ~energy =
    assert (Strictly_positive_int.to_int n_det <=
            Strictly_positive_int.to_int psi_det_size );
    {  n_state; n_det ; psi_det_size ;
       n_det_generators ; n_det_selectors ;
       psi_det ; psi_coef ; energy }
end

(** GetPsiReply_msg : Reply to the GetPsi message *)
module GetPsiReply_msg : sig
  type t = string list
  val create : psi:Psi.t -> t
  val to_string : t -> string
end = struct
  type t = string list
  let create ~psi =
    let g, s = 
      match psi.Psi.n_det_generators, psi.Psi.n_det_selectors with
      | Some g, Some s -> Strictly_positive_int.to_int g, Strictly_positive_int.to_int s
      | _ -> -1, -1
    in
    let head = 
      Printf.sprintf "get_psi_reply %d %d %d %d %d"
        (Strictly_positive_int.to_int psi.Psi.n_state)
        (Strictly_positive_int.to_int psi.Psi.n_det) 
        (Strictly_positive_int.to_int psi.Psi.psi_det_size) 
        g s
    in
    [ head ; psi.Psi.psi_det ; psi.Psi.psi_coef ; psi.Psi.energy ]
  let to_string = function
    | head :: _ :: _ :: _ :: [] -> head
    | _ -> raise (Invalid_argument "Bad wave function message")
end


(** PutPsi : put the current variational wave function *)
module PutPsi_msg : sig
  type t = 
  { client_id :  Id.Client.t ;
    n_state   :  Strictly_positive_int.t ;
    n_det     :  Strictly_positive_int.t ;
    psi_det_size :  Strictly_positive_int.t ;
    n_det_generators : Strictly_positive_int.t option;
    n_det_selectors  : Strictly_positive_int.t option;
    psi       :  Psi.t option }
  val create : 
     client_id:int ->
     n_state:int ->
     n_det:int ->
     psi_det_size:int ->
     psi_det:string option ->
     psi_coef:string option ->
     n_det_generators: int option -> 
     n_det_selectors:int option ->
     energy:string option -> t
  val to_string_list : t -> string list
  val to_string : t -> string 
end = struct
  type t = 
  { client_id :  Id.Client.t ;
    n_state   :  Strictly_positive_int.t ;
    n_det     :  Strictly_positive_int.t ;
    psi_det_size :  Strictly_positive_int.t ;
    n_det_generators : Strictly_positive_int.t option;
    n_det_selectors  : Strictly_positive_int.t option;
    psi       :  Psi.t option }
  let create ~client_id ~n_state ~n_det ~psi_det_size ~psi_det ~psi_coef 
    ~n_det_generators ~n_det_selectors ~energy =
    let n_state, n_det, psi_det_size = 
       Strictly_positive_int.of_int n_state,
       Strictly_positive_int.of_int n_det,
       Strictly_positive_int.of_int psi_det_size
    in
    assert (Strictly_positive_int.to_int psi_det_size >=
      Strictly_positive_int.to_int n_det);
    let n_det_generators, n_det_selectors  =
      match n_det_generators, n_det_selectors  with
      | Some x, Some y -> 
         Some (Strictly_positive_int.of_int x), 
         Some (Strictly_positive_int.of_int y)
      | _ -> None, None
    in
    let psi =
      match (psi_det, psi_coef, energy) with
      | (Some psi_det, Some psi_coef, Some energy) ->
        Some (Psi.create ~n_state ~n_det ~psi_det_size ~psi_det
          ~psi_coef ~n_det_generators ~n_det_selectors ~energy)
      | _ -> None
    in
    { client_id = Id.Client.of_int client_id ;
      n_state ; n_det ; psi_det_size ; n_det_generators ;
      n_det_selectors ; psi }

  let to_string x =
    match x.n_det_generators, x.n_det_selectors with
    | Some g, Some s ->
      Printf.sprintf "put_psi %d %d %d %d %d %d"
        (Id.Client.to_int x.client_id)
        (Strictly_positive_int.to_int x.n_state)
        (Strictly_positive_int.to_int x.n_det) 
        (Strictly_positive_int.to_int x.psi_det_size)  
        (Strictly_positive_int.to_int g)  
        (Strictly_positive_int.to_int s) 
    | _, _ ->
      Printf.sprintf "put_psi %d %d %d %d %d %d"
        (Id.Client.to_int x.client_id)
        (Strictly_positive_int.to_int x.n_state)
        (Strictly_positive_int.to_int x.n_det) 
        (Strictly_positive_int.to_int x.psi_det_size)  
        (-1) (-1)

  let to_string_list x =
    match x.psi with
    | Some psi ->
      [ to_string x ; psi.Psi.psi_det ; psi.Psi.psi_coef ; psi.Psi.energy ]
    | None ->
      [ to_string x ; "None" ; "None" ; "None" ]
end

(** PutPsiReply_msg : Reply to the PutPsi message *)
module PutPsiReply_msg : sig
  type t
  val create : client_id:Id.Client.t -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id :  Id.Client.t ;
  }
  let create ~client_id =
    { client_id; }
  let to_string x =
    Printf.sprintf "put_psi_reply %d"
      (Id.Client.to_int x.client_id)
end


(** GetVector : get the current vector (Davidson) *)
module GetVector_msg : sig
  type t = 
  { client_id: Id.Client.t ;
  }
  val create : client_id:int -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id: Id.Client.t ;
  }
  let create ~client_id = 
    { client_id = Id.Client.of_int client_id }
  let to_string x =
    Printf.sprintf "get_vector %d"
      (Id.Client.to_int x.client_id)
end

module Vector : sig
  type t = 
  {
      size   :  Strictly_positive_int.t;
      data   :  string;
  }
  val create : size:Strictly_positive_int.t -> data:string -> t
end = struct
  type t = 
  {
      size   :  Strictly_positive_int.t;
      data   :  string;
  }
  let create ~size ~data =
    {  size ; data }
end

(** GetVectorReply_msg : Reply to the GetVector message *)
module GetVectorReply_msg : sig
  type t =
  { client_id :  Id.Client.t ;
    vector    :  Vector.t }
  val create : client_id:Id.Client.t -> vector:Vector.t -> t
  val to_string : t -> string
  val to_string_list : t -> string list
end = struct
  type t = 
  { client_id :  Id.Client.t ;
    vector    :  Vector.t }
  let create ~client_id ~vector =
    {  client_id ; vector }
  let to_string x =
    Printf.sprintf "get_vector_reply %d %d"
      (Id.Client.to_int x.client_id)
      (Strictly_positive_int.to_int x.vector.Vector.size)
  let to_string_list x =
    [ to_string x ; x.vector.Vector.data ]
end

(** PutVector : put the current variational wave function *)
module PutVector_msg : sig
  type t = 
  { client_id :  Id.Client.t ;
    size      :  Strictly_positive_int.t ;
    vector    :  Vector.t option;
  }
  val create : 
     client_id:int -> size:int -> data:string option -> t
  val to_string_list : t -> string list
  val to_string : t -> string 
end = struct
  type t = 
  { client_id :  Id.Client.t ;
    size      :  Strictly_positive_int.t ;
    vector    :  Vector.t option;
  }
  let create ~client_id ~size ~data =
    let size =
       Strictly_positive_int.of_int size
    in
    let vector =
      match data with
      | None -> None
      | Some s -> Some (Vector.create ~size ~data:s)
    in
    { client_id = Id.Client.of_int client_id ;
      vector ; size
    }

  let to_string x =
      Printf.sprintf "put_vector %d %d"
        (Id.Client.to_int x.client_id)
        (Strictly_positive_int.to_int x.size)

  let to_string_list x =
    match x.vector with
    | Some v -> [ to_string x ; v.Vector.data ]
    | None -> failwith "Empty vector"
end

(** PutVectorReply_msg : Reply to the PutVector message *)
module PutVectorReply_msg : sig
  type t
  val create : client_id:Id.Client.t -> t
  val to_string : t -> string
end = struct
  type t = 
  { client_id :  Id.Client.t ;
  }
  let create ~client_id =
    { client_id; }
  let to_string x =
    Printf.sprintf "put_vector_reply %d"
      (Id.Client.to_int x.client_id)
end



(** TaskDone : Inform the server that a task is finished *)
module TaskDone_msg : sig
  type t =
    { client_id: Id.Client.t ;
      state:     State.t ;
      task_ids:  Id.Task.t list ;
    }
  val create : state:string -> client_id:int -> task_ids:int list -> t
  val to_string : t -> string
end = struct
  type t =
  { client_id: Id.Client.t ;
    state: State.t ;
    task_ids:  Id.Task.t list;
  }
  let create ~state ~client_id ~task_ids = 
    { client_id = Id.Client.of_int client_id ; 
      state = State.of_string state ;
      task_ids  = List.map ~f:Id.Task.of_int task_ids;
    }

  let to_string x =
    Printf.sprintf "task_done %s %d %s"
      (State.to_string x.state)
      (Id.Client.to_int x.client_id)
      (String.concat ~sep:"|" @@ List.map ~f:Id.Task.to_string x.task_ids)
end

(** Terminate *)
module Terminate_msg : sig
  type t 
  val create : t
  val to_string : t -> string
end = struct
  type t = Terminate
  let create = Terminate
  let to_string x = "terminate"
end

(** Abort *)
module Abort_msg : sig
  type t 
  val create : t
  val to_string : t -> string
end = struct
  type t = Abort
  let create = Abort
  let to_string x = "abort"
end

(** OK *)
module Ok_msg : sig
  type t 
  val create : t
  val to_string : t -> string
end = struct
  type t = Ok
  let create = Ok
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
| GetPsi              of  GetPsi_msg.t
| PutPsi              of  PutPsi_msg.t
| GetPsiReply         of  GetPsiReply_msg.t
| PutPsiReply         of  PutPsiReply_msg.t
| GetVector           of  GetVector_msg.t
| PutVector           of  PutVector_msg.t
| GetVectorReply      of  GetVectorReply_msg.t
| PutVectorReply      of  PutVectorReply_msg.t
| Newjob              of  Newjob_msg.t
| Endjob              of  Endjob_msg.t
| Connect             of  Connect_msg.t
| ConnectReply        of  ConnectReply_msg.t
| Disconnect          of  Disconnect_msg.t
| DisconnectReply     of  DisconnectReply_msg.t
| GetTask             of  GetTask_msg.t
| GetTaskReply        of  GetTaskReply_msg.t
| DelTask             of  DelTask_msg.t
| DelTaskReply        of  DelTaskReply_msg.t
| AddTask             of  AddTask_msg.t
| AddTaskReply        of  AddTaskReply_msg.t
| TaskDone            of  TaskDone_msg.t
| Terminate           of  Terminate_msg.t
| Abort               of  Abort_msg.t
| Ok                  of  Ok_msg.t
| Error               of  Error_msg.t
| SetStopped 
| SetWaiting 
| SetRunning 


let of_string s = 
  let open Message_lexer in
    match parse s with
    | AddTask_  { state ; tasks } -> 
        AddTask (AddTask_msg.create ~state ~tasks)
    | DelTask_  { state ; task_ids } ->
        DelTask (DelTask_msg.create ~state ~task_ids)
    | GetTask_  { state ; client_id } ->
        GetTask (GetTask_msg.create ~state ~client_id)
    | TaskDone_ { state ; task_ids ; client_id } ->
        TaskDone (TaskDone_msg.create ~state ~client_id ~task_ids)
    | Disconnect_ { state ; client_id } ->
        Disconnect (Disconnect_msg.create ~state ~client_id)
    | Connect_ socket ->
        Connect (Connect_msg.create socket)
    | NewJob_ { state ; push_address_tcp ; push_address_inproc } ->
        Newjob (Newjob_msg.create push_address_tcp push_address_inproc state)
    | EndJob_ state  ->
        Endjob (Endjob_msg.create state)
    | GetPsi_ client_id ->
        GetPsi (GetPsi_msg.create ~client_id)
    | PutPsi_ { client_id ; n_state ; n_det ; psi_det_size ; n_det_generators ; n_det_selectors } ->
      begin
        match n_det_selectors, n_det_generators with
        | Some s, Some g -> 
            PutPsi (PutPsi_msg.create ~client_id ~n_state ~n_det ~psi_det_size
                  ~n_det_generators:(Some g) ~n_det_selectors:(Some s)
                  ~psi_det:None ~psi_coef:None ~energy:None )
        | _ ->
            PutPsi (PutPsi_msg.create ~client_id ~n_state ~n_det ~psi_det_size
                  ~n_det_generators:None ~n_det_selectors:None
                  ~psi_det:None ~psi_coef:None ~energy:None )
      end
    | GetVector_ client_id ->
        GetVector (GetVector_msg.create ~client_id)
    | PutVector_ { client_id ; size } ->
        PutVector (PutVector_msg.create ~client_id ~size ~data:None )
    | Terminate_  -> Terminate (Terminate_msg.create )
    | Abort_      -> Abort (Abort_msg.create )
    | SetWaiting_ -> SetWaiting
    | SetStopped_ -> SetStopped
    | SetRunning_ -> SetRunning
    | Ok_ -> Ok (Ok_msg.create)
    | Error_ m -> Error (Error_msg.create m)
  
    

let to_string = function
| GetPsi              x -> GetPsi_msg.to_string             x
| PutPsiReply         x -> PutPsiReply_msg.to_string        x
| GetVector           x -> GetVector_msg.to_string          x
| PutVectorReply      x -> PutVectorReply_msg.to_string     x
| Newjob              x -> Newjob_msg.to_string             x
| Endjob              x -> Endjob_msg.to_string             x
| Connect             x -> Connect_msg.to_string            x
| ConnectReply        x -> ConnectReply_msg.to_string       x
| Disconnect          x -> Disconnect_msg.to_string         x
| DisconnectReply     x -> DisconnectReply_msg.to_string    x
| GetTask             x -> GetTask_msg.to_string            x
| GetTaskReply        x -> GetTaskReply_msg.to_string       x
| DelTask             x -> DelTask_msg.to_string            x
| DelTaskReply        x -> DelTaskReply_msg.to_string       x
| AddTask             x -> AddTask_msg.to_string            x
| AddTaskReply        x -> AddTaskReply_msg.to_string       x
| TaskDone            x -> TaskDone_msg.to_string           x
| Terminate           x -> Terminate_msg.to_string          x
| Abort               x -> Abort_msg.to_string             x
| Ok                  x -> Ok_msg.to_string                 x
| Error               x -> Error_msg.to_string              x
| PutPsi              x -> PutPsi_msg.to_string             x
| GetPsiReply         x -> GetPsiReply_msg.to_string        x
| PutVector           x -> PutVector_msg.to_string          x
| GetVectorReply      x -> GetVectorReply_msg.to_string     x
| SetStopped            -> "set_stopped"
| SetRunning            -> "set_running"
| SetWaiting            -> "set_waiting"


let to_string_list = function
| PutPsi           x -> PutPsi_msg.to_string_list         x
| PutVector        x -> PutVector_msg.to_string_list      x
| GetVectorReply   x -> GetVectorReply_msg.to_string_list x
| _                  -> assert false

