open Core.Std;;

(*
let rec transpose = function
| []          -> []
| []::tail    -> transpose tail
| (x::t1)::t2 ->
    let new_head = (x::(List.map List.hd t2)) 
    and new_tail =  (transpose (t1 :: (List.map List.tl t2) ))
    in 
    new_head @ new_tail
;;
*)

let input_to_sexp s =
    let result = 
      String.split_lines s 
      |> List.filter ~f:(fun x->
         (String.strip x) <> "")
      |> List.map ~f:(fun x->
         "("^(String.tr '=' ' ' x)^")")
      |> String.concat 
    in
    print_endline ("("^result^")");
    "("^result^")"
    |> Sexp.of_string
;;

