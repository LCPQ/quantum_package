open Qptypes;;
open Qputils;;
open Core.Std;;

module Hartree_fock : sig
  type t = 
    { n_it_scf_max       : Strictly_positive_int.t;
      thresh_scf         : Threshold.t;
    } with sexp
  ;;
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { n_it_scf_max       : Strictly_positive_int.t;
      thresh_scf         : Threshold.t;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "hartree_fock";;

  let read_n_it_scf_max () = 
    if not (Ezfio.has_hartree_fock_n_it_scf_max ()) then
       get_default "n_it_scf_max"
       |> Int.of_string
       |> Ezfio.set_hartree_fock_n_it_scf_max
    ;
    Ezfio.get_hartree_fock_n_it_scf_max ()
    |> Strictly_positive_int.of_int
  ;;

  let read_thresh_scf() = 
    if not (Ezfio.has_hartree_fock_thresh_scf()) then
       get_default "thresh_scf"
       |> Float.of_string
       |> Ezfio.set_hartree_fock_thresh_scf
    ;
    Ezfio.get_hartree_fock_thresh_scf ()
    |> Threshold.of_float ;;


  let read () = 
    { n_it_scf_max       = read_n_it_scf_max ();
      thresh_scf         = read_thresh_scf ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
Max number of SCF iterations ::

  n_it_scf_max = %s

SCF convergence criterion (on energy) ::

  thresh_scf = %s

"
        (Strictly_positive_int.to_string b.n_it_scf_max)
        (Threshold.to_string b.thresh_scf)
end


