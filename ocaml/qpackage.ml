open Core.Std;;

(** Variables related to the quantum package installation *)

let root = 
  match (Sys.getenv "QPACKAGE_ROOT") with
  | None -> failwith "QPACKAGE_ROOT environment variable is not set.
Please source the quantum_package.rc file."
  | Some x -> x 
;;

