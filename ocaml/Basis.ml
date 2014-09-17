open Core.Std;;

type t = Gto.t list;;

(** Read all the basis functions of an element *)
let read in_channel =
  let rec read result =
    try
      let gto = Gto.read_one in_channel in
      read (gto::result)
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

let read_element in_channel element =
  ignore (find in_channel element) ;
  read in_channel ;
;;

let to_string b =
  List.map ~f:Gto.to_string b
  |> String.concat ~sep:"\n"
;;
