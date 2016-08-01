open Core.Std
open Qptypes


type t =
{ queued         : Id.Task.t list ;
  running        : (Id.Task.t, Id.Client.t) Map.Poly.t ;
  tasks          : (Id.Task.t, string)      Map.Poly.t;
  clients        : Id.Client.t Set.Poly.t;
  next_client_id : Id.Client.t;
  next_task_id   : Id.Task.t;
}



let create () =
  { queued         = [] ; 
    running        = Map.Poly.empty ;
    tasks          = Map.Poly.empty;
    clients        = Set.Poly.empty;
    next_client_id = Id.Client.of_int 1;
    next_task_id   = Id.Task.of_int 1;
  }




let add_task ~task q =
  let task_id =
    q.next_task_id 
  in
  { q with
    queued = task_id :: q.queued ;
    tasks  = Map.add q.tasks ~key:task_id ~data:task ;
    next_task_id = Id.Task.increment task_id ;
  }




let add_client q =
  let client_id =
    q.next_client_id 
  in
  { q with
    clients = Set.add q.clients client_id;
    next_client_id = Id.Client.increment client_id;
  }, client_id


let pop_task ~client_id q = 
  let { queued ; running ; _ } =
    q
  in
  assert (Set.mem q.clients client_id);
  match queued with
  | task_id :: new_queue ->
    let new_q =
      { q with
        queued = new_queue ;
        running = Map.add running ~key:task_id ~data:client_id ;
      }
    in new_q, Some task_id, (Map.find q.tasks task_id)
  | [] -> q, None, None
    

let del_client ~client_id q =
  assert (Set.mem q.clients client_id);
  { q with 
    clients = Set.remove q.clients client_id }


let end_task ~task_id ~client_id q = 
  let { running ; tasks ; _ } =
    q
  in
  assert (Set.mem q.clients client_id);
  let () = 
    match Map.Poly.find running task_id with
    | None -> failwith "Task already finished"
    | Some client_id_check -> assert (client_id_check = client_id)
  in
  { q with
    running  = Map.remove running task_id ;
  }
    
let del_task ~task_id q = 
  let { tasks ; _ } =
    q
  in
  
  if (Map.mem tasks task_id) then
      { q with
        tasks    = Map.remove tasks task_id ;
      }
  else
      Printf.sprintf "Task %d is already deleted" (Id.Task.to_int task_id)
      |> failwith

    

let number q =
  Map.length q.tasks

let number_of_queued q =
  List.length q.queued

let number_of_running q =
  Map.length q.running


let to_string { queued ; running ; tasks ; _ } =
  let q =
    List.map ~f:Id.Task.to_string queued
    |> String.concat ~sep:" ; "
  and r =
    Map.Poly.to_alist running
    |> List.map ~f:(fun (t,c) -> "("^(Id.Task.to_string t)^", "
       ^(Id.Client.to_string c)^")") 
    |> String.concat ~sep:" ; "
  and t = 
    Map.Poly.to_alist tasks
    |> List.map ~f:(fun (t,c) -> "("^(Id.Task.to_string t)^", \""
       ^c^"\")") 
    |> String.concat ~sep:" ; "
  in
  Printf.sprintf "{
queued   : { %s }
running  : { %s }
tasks    : [ %s
           ]
}" q r t

  
