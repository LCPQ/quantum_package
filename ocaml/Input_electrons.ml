open Qptypes;;
open Qputils;;
open Core.Std;;

module Electrons : sig
  type t = 
    { elec_alpha_num     : Strictly_positive_int.t;
      elec_beta_num      : Positive_int.t;
      elec_num           : Strictly_positive_int.t;
    }
  ;;
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { elec_alpha_num     : Strictly_positive_int.t;
      elec_beta_num      : Positive_int.t;
      elec_num           : Strictly_positive_int.t;
    }
  ;;

  let get_default = Qpackage.get_ezfio_default "electrons";;

  let read_elec_alpha_num() = 
    Ezfio.get_electrons_elec_alpha_num ()
    |> Strictly_positive_int.of_int
  ;;

  let read_elec_beta_num() = 
    Ezfio.get_electrons_elec_beta_num ()
    |> Positive_int.of_int
  ;;

  let read_elec_num () = 
    let na = Ezfio.get_electrons_elec_alpha_num ()
    and nb = Ezfio.get_electrons_elec_beta_num  ()
    in assert (na >= nb);
    Strictly_positive_int.of_int (na + nb)
  ;;


  let read () = 
    { elec_alpha_num      = read_elec_alpha_num ();
      elec_beta_num       = read_elec_beta_num ();
      elec_num            = read_elec_num ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
elec_alpha_num     = %s
elec_beta_num      = %s
elec_num           = %s
"
        (Strictly_positive_int.to_string b.elec_alpha_num)
        (Positive_int.to_string b.elec_beta_num)
        (Strictly_positive_int.to_string b.elec_num)
end


