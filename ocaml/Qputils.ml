open Core.Std

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

let rmdir dirname =
  let rec remove_one dir =
    Sys.chdir dir;
    Sys.readdir "."
    |> Array.iter ~f:(fun x ->
      match (Sys.is_directory x, Sys.is_file x) with
      | (`Yes, _) -> remove_one x
      | (_, `Yes) -> Sys.remove x
      | _ -> failwith ("Unable to remove file "^x^".")
    );
    Sys.chdir "..";
    Unix.rmdir dir
  in
  remove_one dirname
  
    

