open Core.Std;;

type t = {
  x : float ;
  y : float ;
  z : float ;
}

let of_string s =
  let l = s
  |> String.split ~on:' '
  |> List.filter ~f:(fun x -> x <> "")
  |> List.map ~f:Float.of_string
  |> Array.of_list
  in
  { x = l.(0) ;
    y = l.(1) ;
    z = l.(2) }
;;
           

let distance2 p1 p2 =
  let { x=x1 ; y=y1 ; z=z1 } = p1
  and { x=x2 ; y=y2 ; z=z2 } = p2 in
  (x2-.x1)*.(x2-.x1) +. (y2-.y1)*.(y2-.y1) +. (z2-.z1)*.(z2-.z1) 
;;

let distance p1 p2 = sqrt (distance2 p1 p2)
;;

let to_string p =
  let { x=x ; y=y ; z=z } = p in
  Printf.sprintf "%f  %f  %f" x y z
;;
  
