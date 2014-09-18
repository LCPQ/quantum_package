open Core.Std;;

exception AtomError of string

module Charge : sig
  type t 
  val to_float : t -> float
  val to_int   : t -> int
  val to_string: t -> string 
  val of_float : float -> t
  val of_int   : int -> t
  val of_string: string -> t
end = struct
  type t = float
  let to_float x = x
  let to_int   x = Float.to_int x
  let to_string x = Float.to_string (to_float x)
  let of_float x = x
  let of_int   i = Float.of_int i
  let of_string s = Float.of_string s
end

type t =
{ element : Element.t ;
  charge  : Charge.t ;
  coord   : Point3d.t ;
}

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
      charge  = Charge.of_int (Element.charge e);
      coord   = Point3d.of_string u (String.concat [x; y; z] ~sep:" ")
    }
  | _ -> raise (AtomError s)
;;
  
let to_string a =
  [ Element.to_string a.element ;
    Charge.to_string  a.charge ;
    Point3d.to_string a.coord ]
  |> String.concat ?sep:(Some "   ")
;;

