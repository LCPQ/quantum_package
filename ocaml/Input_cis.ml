open Qptypes;;
open Qputils;;
open Core.Std;;

module Cis_dressed : sig
  type t = 
    { n_state_cis        : States_number.t;
      n_core_cis         : Positive_int.t;
      n_act_cis          : Positive_int.t;
      mp2_dressing       : bool;
      standard_doubles   : bool;
      en_2_2             : bool;
    } with sexp
  ;;
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { n_state_cis        : States_number.t;
      n_core_cis         : Positive_int.t;
      n_act_cis          : Positive_int.t;
      mp2_dressing       : bool;
      standard_doubles   : bool;
      en_2_2             : bool;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "cis_dressed";;

  let read_n_state_cis () = 
    if not (Ezfio.has_cis_dressed_n_state_cis ()) then
       get_default "n_state_cis"
       |> Int.of_string
       |> Ezfio.set_cis_dressed_n_state_cis
    ;
    Ezfio.get_cis_dressed_n_state_cis ()
    |> States_number.of_int
  ;;

  let read_n_core_cis () = 
    if not (Ezfio.has_cis_dressed_n_core_cis ()) then
       get_default "n_core_cis"
       |> Int.of_string
       |> Ezfio.set_cis_dressed_n_core_cis
    ;
    Ezfio.get_cis_dressed_n_core_cis ()
    |> Positive_int.of_int
  ;;

  let read_n_act_cis () = 
    if not (Ezfio.has_cis_dressed_n_act_cis ()) then
       Ezfio.get_mo_basis_mo_tot_num ()
       |> Ezfio.set_cis_dressed_n_act_cis
    ;
    Ezfio.get_cis_dressed_n_act_cis ()
    |> Positive_int.of_int
  ;;

  let read_mp2_dressing () = 
    if not (Ezfio.has_cis_dressed_mp2_dressing ()) then
       get_default "mp2_dressing"
       |> Bool.of_string
       |> Ezfio.set_cis_dressed_mp2_dressing
    ;
    Ezfio.get_cis_dressed_mp2_dressing ()
  ;;

  let read_standard_doubles () = 
    if not (Ezfio.has_cis_dressed_standard_doubles ()) then
       get_default "standard_doubles"
       |> Bool.of_string
       |> Ezfio.set_cis_dressed_standard_doubles
    ;
    Ezfio.get_cis_dressed_standard_doubles ()
  ;;

  let read_en_2_2 () = 
    if not (Ezfio.has_cis_dressed_en_2_2 ()) then
       get_default "en_2_2"
       |> Bool.of_string
       |> Ezfio.set_cis_dressed_en_2_2
    ;
    Ezfio.get_cis_dressed_en_2_2 ()
  ;;

  let read () = 
    { n_state_cis        = read_n_state_cis ();
      n_core_cis         = read_n_core_cis ();
      n_act_cis          = read_n_act_cis ();
      mp2_dressing       = read_mp2_dressing ();
      standard_doubles   = read_standard_doubles ();
      en_2_2             = read_en_2_2 ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
n_state_cis        = %s
n_core_cis         = %s
n_act_cis          = %s
mp2_dressing       = %s
standard_doubles   = %s
en_2_2             = %s
"
        (States_number.to_string b.n_state_cis)
        (Positive_int.to_string b.n_core_cis)
        (Positive_int.to_string b.n_act_cis)
        (Bool.to_string b.mp2_dressing)
        (Bool.to_string b.standard_doubles)
        (Bool.to_string b.en_2_2)
end


