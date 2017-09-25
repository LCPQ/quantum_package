open Core

let filenames =
  let dir_name = Qpackage.root^"/data/basis/"
  in
  Sys.readdir dir_name
  |> Array.map ~f:(fun x -> dir_name^x)
  |> Array.to_list
;;

let clean_file filename =
  let command =
    Printf.sprintf "cp -f %s %s.old" filename filename
  in 
  let () = 
    match Sys.command command with
    | 0 -> ()
    | i -> failwith (Printf.sprintf "Command %s exited with code %d\n" command i)
  in

  let lines =
    In_channel.with_file filename ~f:In_channel.input_lines
  in

  Out_channel.with_file filename ~f:(fun out_channel ->

    let rec loop ~do_s = function
    | [] -> ()
    | line :: tail ->
      begin
        let buffer = String.strip line
            |> String.split ~on:' ' 
            |> List.filter ~f:(fun x -> x <> "")
        in
        let () = 
          match buffer with
          | []       -> Printf.fprintf out_channel "\n"
          | [ atom ] -> Printf.fprintf out_channel "%s\n" atom
          | [ i ; expo ; coef ]  ->
              Printf.fprintf out_channel "%3s  %14s  %14s\n" i expo coef
          | [ i ; expo ; coef ; coef2 ]  ->
              if (do_s) then
                Printf.fprintf out_channel "%3s  %14s  %14s\n" i expo coef
              else
                Printf.fprintf out_channel "%3s  %14s  %14s\n" i expo coef2
          | [ sym ; n ] -> 
              if (sym = "L") then
                 let () = 
                   Printf.fprintf out_channel "S %3s\n" n
                 in
                 let rec build_newlist accu = function
                   | (0, _)  
                   | (_,[])  -> List.rev ((Printf.sprintf "P %3s\n" n)::accu)
                   | (i,head::tail) ->
                      build_newlist (head::accu) ( i-1, tail )
                 in
                 let newlist = build_newlist [] ((Int.of_string n),tail)
                 in
                 loop ~do_s:true newlist
              else
                 Printf.fprintf out_channel "%s %3s\n" sym n
          | _ -> ()
        in
        loop ~do_s:do_s tail
      end
    in loop ~do_s:false lines
  )
;;

List.iter ~f:clean_file filenames
;;

