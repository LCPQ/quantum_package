open Core.Std;;
open Qptypes;;

type t = (Gto.t * Nucl_number.t) list with sexp;;

(** Read all the basis functions of an element *)
let read in_channel at_number =
  let rec read result =
    try
      let gto = Gto.read_one in_channel in
      read ( (gto,at_number)::result)
    with
    | Gto.End_Of_Basis -> List.rev result
  in read []  
;;

(** Find an element in the basis set file *)
let find in_channel element =
  In_channel.seek in_channel 0L;
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

(** Read an element from the file *)
let read_element in_channel at_number element =
  ignore (find in_channel element) ;
  read in_channel at_number ;
;;

let to_string b =
  let new_nucleus n = 
    Printf.sprintf "Atom %d" n
  in
  
  let rec do_work accu current_nucleus = function
  | [] -> List.rev accu
  | (g,n)::tail -> 
    let n = Nucl_number.to_int n
    in
    let accu = 
       if (n <> current_nucleus) then
         (new_nucleus n)::""::accu
       else
         accu
    in
    do_work ((Gto.to_string g)::accu) n tail
  in
  do_work [new_nucleus 1] 1 b
  |> String.concat ~sep:"\n"
;;

include To_md5;;
let to_md5 = to_md5 sexp_of_t
;;

