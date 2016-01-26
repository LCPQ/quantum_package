open Core.Std
open Qputils
open Qptypes
open Symmetry

let () =
  "SPDFGHIJKL"
  |> String.to_list_rev
  |> List.rev
  |> List.map ~f:of_char
  |> List.map ~f:Xyz.of_symmetry
  |> List.iter ~f:(fun x -> List.iter x ~f:(fun y -> Xyz.to_string y |> print_endline) ;
     print_newline ();)


