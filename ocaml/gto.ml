open Core.Std;;
open Qptypes;;

exception GTO_Read_Failure of string
exception End_Of_Basis

type t =
{ sym  : Symmetry.t ;
  lc   : ((Primitive.t * float) list)
}
;;

let of_prim_coef_list pc =
  let (p,c) = List.hd_exn pc in
  let sym = p.Primitive.sym in
  let rec check = function
  | [] -> `OK
  | (p,c)::tl -> 
      if p.Primitive.sym <> sym then
        `Failed
      else
        check tl
  in
  match check pc with
  | `Failed -> raise (Failure "Failed in of_prim_coef_list")
  | `OK ->
  { sym = sym ;
    lc  = pc
  }
;;


let read_one in_channel =
  (* Fetch number of lines to read on first line *)
  let buffer = input_line in_channel in
  if ( (String.strip buffer) = "" ) then
     raise End_Of_Basis;
  let sym_str = String.sub buffer 0 2 in
  let   n_str = String.sub buffer 2 ((String.length buffer)-2) in
  let sym = Symmetry.of_string (String.strip sym_str) in
  let n = Int.of_string (String.strip n_str) in
  (* Read all the primitives *)
  let rec read_lines result = function
  | 0 -> result
  | i ->
    begin
      let line_buffer = input_line in_channel in
      let buffer = line_buffer 
      |> String.split ~on:' ' 
      |> List.filter ~f:(fun x -> x <> "")
      in
      match buffer with
      | [ j ; expo ; coef ] ->
        begin
          let p = { Primitive.sym = sym ;
                    Primitive.expo = Positive_float.of_float
                        (Float.of_string expo)
                  }
          and c = Float.of_string coef in
          read_lines ( (p,c)::result) (i-1)
        end
      | _ -> raise (GTO_Read_Failure line_buffer)
    end
  in read_lines [] n
  |> of_prim_coef_list
;;

(** Read all the basis functions of an element *)
let read_basis in_channel =
  let rec read result =
    try
      let gto = read_one in_channel in
      read (gto::result)
    with
    | End_Of_Basis -> List.rev result
  in read []  
;;

(** Find an element in the basis set file *)
let find in_channel element =
  let element_read = ref Element.X in
  while !element_read <> element
  do
    let buffer = input_line in_channel in
    try
      element_read := Element.of_string buffer
    with
    | Element.ElementError _ -> ()
  done ;
  !element_read
;;

let read_basis_of_element in_channel element =
  find in_channel element ;
  read_basis in_channel ;
;;

(** Transform the gto to a string *)
let to_string { sym = sym ; lc = lc } =
  let f (p,c) = Printf.sprintf "( %s, %f )" (Primitive.to_string p) c
  in
  Printf.sprintf "[ %s : %s ]" (Symmetry.to_string sym)
    (String.concat (List.map ~f:f lc) ?sep:(Some ", "))
;;
