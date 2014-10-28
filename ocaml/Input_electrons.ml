open Qptypes;;
open Qputils;;
open Core.Std;;

module Electrons : sig
  type t = 
    { elec_alpha_num     : Elec_alpha_number.t;
      elec_beta_num      : Elec_beta_number.t;
      elec_num           : Elec_number.t;
    } with sexp
  ;;
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { elec_alpha_num     : Elec_alpha_number.t;
      elec_beta_num      : Elec_beta_number.t;
      elec_num           : Elec_number.t;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "electrons";;

  let read_elec_alpha_num() = 
    Ezfio.get_electrons_elec_alpha_num ()
    |> Elec_alpha_number.of_int
  ;;

  let read_elec_beta_num() = 
    Ezfio.get_electrons_elec_beta_num ()
    |> Elec_beta_number.of_int
  ;;

  let read_elec_num () = 
    let na = Ezfio.get_electrons_elec_alpha_num ()
    and nb = Ezfio.get_electrons_elec_beta_num  ()
    in assert (na >= nb);
    Elec_number.of_int (na + nb)
  ;;


  let read () = 
    { elec_alpha_num      = read_elec_alpha_num ();
      elec_beta_num       = read_elec_beta_num ();
      elec_num            = read_elec_num ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
Spin multiplicity is %s.

Number of alpha and beta electrons ::

  elec_alpha_num = %s
  elec_beta_num  = %s

"
        (Multiplicity.of_alpha_beta b.elec_alpha_num b.elec_beta_num
         |> Multiplicity.to_string)
        (Elec_alpha_number.to_string b.elec_alpha_num)
        (Elec_beta_number.to_string b.elec_beta_num)
  ;;

  let debug b =
    Printf.sprintf "elec_alpha_num     = %s
elec_beta_num      = %s
elec_num           = %s
"
        (Elec_alpha_number.to_string b.elec_alpha_num)
        (Elec_beta_number.to_string b.elec_beta_num)
        (Elec_number.to_string b.elec_num)
  ;;
end


