open Core.Std;;

exception AtomError of string

type t =
{ element : Element.t ;
  charge  : Charge.t ;
  coord   : Point3d.t ;
} with sexp

(** Read xyz coordinates of the atom with unit u *)
let of_string u s =
  let buffer = s
  |> String.split ~on:' '
  |> List.filter ~f:(fun x -> x <> "")
  in
  match buffer with
  | [ name; charge; x; y; z ] ->
    { element = Element.of_string name ;
      charge  = Charge.of_string  charge ;
      coord   = Point3d.of_string u (String.concat [x; y; z] ~sep:" ")
    }
  | [ name; x; y; z ] ->
    let e = Element.of_string name in
    { element = e ;
      charge  = Element.to_charge e;
      coord   = Point3d.of_string u (String.concat [x; y; z] ~sep:" ")
    }
  | _ -> raise (AtomError s)
;;
  
let to_string u a =
  [ Element.to_string a.element ;
    Charge.to_string  a.charge ;
    Point3d.to_string u a.coord ]
  |> String.concat ~sep:"   "
;;

