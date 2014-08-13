let test_module () = 
  let atom = Elements.of_string "Cobalt" in
  Printf.printf "%s %d\n" (Elements.to_string atom) (Elements.charge atom)
;;

test_module ();;
