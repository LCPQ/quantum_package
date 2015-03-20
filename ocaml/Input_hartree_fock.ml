open Qptypes;;
open Qputils;;
open Core.Std;;

module Hartree_fock : sig
  type t = 
    { n_it_scf_max       : Strictly_positive_int.t;
      thresh_scf         : Threshold.t;
      guess              : MO_guess.t;
    } with sexp
  ;;
  val read  : unit -> t option
  val write : t -> unit
  val to_string : t -> string
  val to_rst : t -> Rst_string.t
  val of_rst : Rst_string.t -> t  option
end = struct
  type t = 
    { n_it_scf_max       : Strictly_positive_int.t;
      thresh_scf         : Threshold.t;
      guess              : MO_guess.t;
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

  let write_n_it_scf_max n_it_scf_max = 
    Strictly_positive_int.to_int n_it_scf_max
    |> Ezfio.set_hartree_fock_n_it_scf_max
  ;;

  let read_thresh_scf () = 
    if not (Ezfio.has_hartree_fock_thresh_scf()) then
       get_default "thresh_scf"
       |> Float.of_string
       |> Ezfio.set_hartree_fock_thresh_scf
    ;
    Ezfio.get_hartree_fock_thresh_scf ()
    |> Threshold.of_float
  ;;

  let write_thresh_scf thresh_scf =
    Threshold.to_float thresh_scf
    |> Ezfio.set_hartree_fock_thresh_scf
  ;;

  let read_guess () = 
    if not (Ezfio.has_hartree_fock_guess ()) then
      get_default "guess"
      |> String.strip ~drop:(fun x -> x = '"')
      |> Ezfio.set_hartree_fock_guess
    ;
    Ezfio.get_hartree_fock_guess ()
    |> MO_guess.of_string
  ;;

  let write_guess guess =
    MO_guess.to_string guess
    |> Ezfio.set_hartree_fock_guess
  ;;

  let read () = 
    Some
    { n_it_scf_max       = read_n_it_scf_max ();
      thresh_scf         = read_thresh_scf ();
      guess              = read_guess ();
    }
  ;;


  let write { n_it_scf_max ;
              thresh_scf ;
              guess      ;
            } =
    write_n_it_scf_max n_it_scf_max;
    write_thresh_scf thresh_scf; 
    write_guess  guess
  ;;


  let to_string b =
    Printf.sprintf "
n_it_scf_max = %s
thresh_scf  = %s
guess       = %s
"
      (Strictly_positive_int.to_string b.n_it_scf_max)
      (Threshold.to_string b.thresh_scf)
      (MO_guess.to_string b.guess)
  ;;

  let to_rst b =
    Printf.sprintf "
Type of MO guess [ Huckel | HCore ] ::

  guess = %s

Max number of SCF iterations ::

  n_it_scf_max = %s

SCF convergence criterion (on energy) ::

  thresh_scf = %s

"
      (MO_guess.to_string b.guess)
      (Strictly_positive_int.to_string b.n_it_scf_max)
      (Threshold.to_string b.thresh_scf)
    |> Rst_string.of_string
  ;;

  include Generic_input_of_rst;;
  let of_rst = of_rst t_of_sexp;;

end


