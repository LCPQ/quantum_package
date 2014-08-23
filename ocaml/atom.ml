open Core.Std;;

exception AtomError of string

module Charge : sig
  type t 
  val to_float : t -> float
  val to_string: t -> string 
  val of_float : float -> t
  val of_int   : int -> t
  val of_string: string -> t
end = struct
  type t = float
  let to_float x = x
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

let of_string s =
  let buffer = s
  |> String.split ~on:' '
  |> List.filter ~f:(fun x -> x <> "")
  in
  match buffer with
  | [ name; charge; x; y; z ] ->
    { element = Element.of_string name ;
      charge  = Charge.of_string  charge ;
      coord   = Point3d.of_string (String.concat [x; y; z] ?sep:(Some " "))
    }
  | _ -> raise (AtomError s)
;;
  
let to_string a =
  [ Element.to_string a.element ;
    Charge.to_string  a.charge ;
    Point3d.to_string a.coord ]
  |> String.concat ?sep:(Some "  ")
;;

