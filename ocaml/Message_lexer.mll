{

type kw_type = 
    | TEXT     of string
    | WORD     of string
    | INTEGER  of int
    | FLOAT    of float
    | NONE
    | ADD_TASK 
    | DEL_TASK 
    | GET_TASK 
    | TASK_DONE
    | DISCONNECT
    | CONNECT
    | NEW_JOB
    | END_JOB
    | TERMINATE
    | ABORT
    | GET_PSI
    | PUT_PSI
    | GET_VECTOR
    | PUT_VECTOR
    | OK
    | ERROR
    | SET_STOPPED
    | SET_RUNNING
    | SET_WAITING

type state_tasks            = { state : string ; tasks            : string list ; }
type state_taskids          = { state : string ; task_ids         : int list   ; }
type state_taskids_clientid = { state : string ; task_ids         : int list   ; client_id : int    ; }
type state_clientid         = { state : string ; client_id        : int    ; }
type state_tcp_inproc       = { state : string ; push_address_tcp : string ; push_address_inproc : string ; }
type psi = { client_id: int ; n_state: int ; n_det: int ; psi_det_size: int ; 
  n_det_generators: int option ; n_det_selectors: int option ; }
type vector = { client_id: int ; size: int }

type msg =
    | AddTask_    of state_tasks
    | DelTask_    of state_taskids
    | GetTask_    of state_clientid
    | TaskDone_   of state_taskids_clientid
    | Disconnect_ of state_clientid
    | Connect_    of string
    | NewJob_     of state_tcp_inproc
    | EndJob_     of string
    | Terminate_
    | Abort_
    | GetPsi_     of int
    | PutPsi_     of psi
    | GetVector_  of int
    | PutVector_  of vector
    | Ok_
    | Error_      of string 
    | SetStopped_
    | SetRunning_ 
    | SetWaiting_ 
}

let word = [^' ' '\t' '\n']+
let text = [^ ' ' '|']+[^ '|']+
let integer  = ['0'-'9']+ 
let real = '-'? integer '.' integer (['e' 'E'] '-'? integer)? 

let white = [' ' '\t']+


rule get_text = parse
  | text    as t   { TEXT t }
  | eof            { TERMINATE }
  | _              { NONE }

and get_int = parse
  | integer as i   { INTEGER (int_of_string i) }
  | eof            { TERMINATE }
  | _              { NONE }

and get_word = parse
  | word    as w   { WORD w }
  | eof            { TERMINATE }
  | _              { NONE }

and kw = parse
  | "add_task"     { ADD_TASK }
  | "del_task"     { DEL_TASK }
  | "get_task"     { GET_TASK }
  | "task_done"    { TASK_DONE }
  | "disconnect"   { DISCONNECT }
  | "connect"      { CONNECT }
  | "new_job"      { NEW_JOB }
  | "end_job"      { END_JOB }
  | "terminate"    { TERMINATE }
  | "abort"        { ABORT }
  | "get_psi"      { GET_PSI }
  | "put_psi"      { PUT_PSI }
  | "get_vector"   { GET_PSI }
  | "put_vector"   { PUT_PSI }
  | "ok"           { OK }
  | "error"        { ERROR }
  | "set_stopped"  { SET_STOPPED }
  | "set_running"  { SET_RUNNING }
  | "set_waiting"  { SET_WAITING }
  | _              { NONE }


{
  let rec read_text ?(accu=[]) lexbuf =
    let token =
      get_text lexbuf
    in
    match token with
    | TEXT t -> read_text ~accu:(t::accu) lexbuf
    | TERMINATE -> List.rev accu 
    | NONE -> read_text ~accu lexbuf
    | _ -> failwith "Error in MessageLexer (2)"

  and read_word lexbuf =
    let token =
      get_word lexbuf
    in
    match token with
    | WORD w -> w
    | NONE -> read_word lexbuf
    | _ -> failwith "Error in MessageLexer (3)"

  and read_int lexbuf =
    let token =
      get_int lexbuf
    in
    match token with
    | INTEGER i -> i
    | NONE -> read_int lexbuf
    | _ -> failwith "Error in MessageLexer (4)"

  and read_ints ?(accu=[]) lexbuf =
    let token =
      get_int lexbuf
    in
    match token with
    | INTEGER i -> read_ints ~accu:(i::accu) lexbuf
    | TERMINATE -> List.rev accu
    | NONE -> read_ints ~accu lexbuf
    | _ -> failwith "Error in MessageLexer (4)"

  and parse_rec lexbuf =
    let token =
      kw lexbuf
    in
    match token with
    | ADD_TASK -> 
        let state = read_word lexbuf in
        let tasks  = read_text lexbuf in
        AddTask_ { state ; tasks }

    | DEL_TASK ->
        let state    = read_word lexbuf in  
        let task_ids = read_ints lexbuf in
        DelTask_ { state ; task_ids }
 
    | GET_TASK ->
        let state   = read_word lexbuf in  
        let client_id = read_int lexbuf in
        GetTask_ { state ; client_id }
 
    | TASK_DONE ->
        let state     = read_word lexbuf in  
        let client_id = read_int  lexbuf in
        let task_ids  = read_ints lexbuf in
        TaskDone_ { state ; task_ids ; client_id }
 
    | DISCONNECT ->
        let state     = read_word lexbuf in  
        let client_id = read_int lexbuf in
        Disconnect_ { state ; client_id }
 
    | GET_PSI ->
        let client_id = read_int lexbuf in
        GetPsi_ client_id
 
    | PUT_PSI ->
        let client_id    = read_int lexbuf in
        let n_state      = read_int lexbuf in
        let n_det        = read_int lexbuf in
        let psi_det_size = read_int lexbuf in
        let n_det_generators, n_det_selectors = 
          try
            (Some (read_int lexbuf), Some (read_int lexbuf))
          with (Failure _) -> (None, None)
        in
        PutPsi_ { client_id ; n_state ; n_det ; psi_det_size ; n_det_generators ; n_det_selectors }
 
    | GET_VECTOR ->
        let client_id = read_int lexbuf in
        GetVector_ client_id
 
    | PUT_VECTOR ->
        let client_id    = read_int lexbuf in
        let size         = read_int lexbuf in
        PutVector_ { client_id ; size }
 
    | CONNECT ->
        let socket    = read_word lexbuf in  
        Connect_ socket
 
    | NEW_JOB ->
        let state               = read_word lexbuf in
        let push_address_tcp    = read_word lexbuf in
        let push_address_inproc = read_word lexbuf in
        NewJob_ { state ; push_address_tcp ; push_address_inproc }
 
    | END_JOB ->
        let state  = read_word lexbuf in  
        EndJob_ state
 
    | ERROR       ->
        let message = List.hd (read_text lexbuf) in
        Error_ message

    | OK          -> Ok_
    | SET_WAITING -> SetWaiting_
    | SET_RUNNING -> SetRunning_
    | SET_STOPPED -> SetStopped_
    | TERMINATE   -> Terminate_
    | ABORT       -> Abort_
    | NONE        -> parse_rec lexbuf
    | _ -> failwith "Error in MessageLexer"

  let parse message = 
    let lexbuf =
      Lexing.from_string message
    in
    parse_rec lexbuf


  let debug () =
    let l = [
      "add_task  state_pouet  Task pouet zob" ;
      "add_task  state_pouet  Task pouet zob |Task2 zob | Task3 prout" ;
      "del_task  state_pouet  12345" ;
      "del_task  state_pouet  12345 | 6789 | 10 | 11" ;
      "get_task  state_pouet  12" ;
      "task_done state_pouet  12 12345";
      "task_done state_pouet  12 12345 | 678 | 91011";
      "connect tcp";
      "disconnect state_pouet 12";
      "new_job state_pouet tcp://test.com:12345 ipc:///dev/shm/x.socket";
      "end_job state_pouet";
      "terminate" ;
      "abort" ;
      "set_running" ;
      "set_stopped" ;
      "set_waiting" ;
      "ok" ;
      "error my_error" ;
      "get_psi 12" ;
      "put_psi 12 2 1000 10000 800 900" ;
      "put_psi 12 2 1000 10000"
      ]
      |> List.map parse 
    in
    List.map (function
      | AddTask_  { state ; tasks   } -> Printf.sprintf "ADD_TASK state:\"%s\" tasks:{\"%s\"}" state (String.concat "\"}|{\"" tasks)
      | DelTask_  { state ; task_ids } -> Printf.sprintf "DEL_TASK state:\"%s\" task_ids:{%s}" state (String.concat "|" @@ List.map string_of_int task_ids)
      | GetTask_  { state ; client_id } -> Printf.sprintf "GET_TASK state:\"%s\" task_id:%d" state client_id
      | TaskDone_ { state ; task_ids ; client_id } -> Printf.sprintf "TASK_DONE state:\"%s\" task_ids:{%s} client_id:%d" state (String.concat "|" @@ List.map string_of_int task_ids) client_id
      | Disconnect_ { state ; client_id } -> Printf.sprintf "DISCONNECT state:\"%s\" client_id:%d" state client_id
      | Connect_ socket -> Printf.sprintf "CONNECT socket:\"%s\"" socket
      | NewJob_ { state ; push_address_tcp ; push_address_inproc } -> Printf.sprintf "NEW_JOB state:\"%s\" tcp:\"%s\" inproc:\"%s\"" state push_address_tcp push_address_inproc
      | EndJob_ state  -> Printf.sprintf "END_JOB state:\"%s\"" state
      | GetPsi_ client_id -> Printf.sprintf "GET_PSI client_id:%d" client_id
      | PutPsi_ { client_id ; n_state ; n_det ; psi_det_size ; n_det_generators ; n_det_selectors } ->
        begin 
          match n_det_selectors, n_det_generators with
          | Some s, Some g ->  Printf.sprintf "PUT_PSI client_id:%d n_state:%d n_det:%d psi_det_size:%d n_det_generators:%d n_det_selectors:%d" client_id n_state n_det psi_det_size g s
          | _ -> Printf.sprintf "PUT_PSI client_id:%d n_state:%d n_det:%d psi_det_size:%d" client_id n_state n_det psi_det_size 
        end
      | GetVector_ client_id -> Printf.sprintf "GET_VECTOR client_id:%d" client_id
      | PutVector_ { client_id ; size } ->
          Printf.sprintf "PUT_VECTOR client_id:%d size:%d" client_id size 
      | Terminate_ ->  "TERMINATE"
      | Abort_ ->  "ABORT"
      | SetWaiting_ ->  "SET_WAITING"
      | SetStopped_ ->  "SET_STOPPED"
      | SetRunning_ ->  "SET_RUNNING"
      | Ok_ ->  "OK"
      | Error_ s ->  Printf.sprintf "ERROR: \"%s\"" s
    ) l
    |> List.iter print_endline

}
