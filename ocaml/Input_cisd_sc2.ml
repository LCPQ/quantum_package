open Qptypes;;
open Qputils;;
open Core.Std;;

module Cisd_sc2 : sig
  type t
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { n_det_max_cisd_sc2 : Strictly_positive_int.t;
      pt2_max            : Positive_float.t;
      do_pt2_end         : bool;
    }
  ;;

  let get_default = Qpackage.get_ezfio_default "cisd_sc2_selected";;

  let read_n_det_max_cisd_sc2 () = 
    if not (Ezfio.has_cisd_sc2_selected_n_det_max_cisd_sc2 ()) then
       get_default "n_det_max_cisd_sc2"
       |> Int.of_string
       |> Ezfio.set_cisd_sc2_selected_n_det_max_cisd_sc2 
    ;
    Ezfio.get_cisd_sc2_selected_n_det_max_cisd_sc2 ()
    |> Strictly_positive_int.of_int
  ;;


  let read_pt2_max () = 
    if not (Ezfio.has_cisd_sc2_selected_pt2_max ()) then
       get_default "pt2_max"
       |> Float.of_string
       |> Ezfio.set_cisd_sc2_selected_pt2_max
    ;
    Ezfio.get_cisd_sc2_selected_pt2_max ()
    |> Positive_float.of_float
  ;;


  let read_do_pt2_end () = 
    if not (Ezfio.has_cisd_sc2_selected_do_pt2_end ()) then
       get_default "do_pt2_end"
       |> Bool.of_string
       |> Ezfio.set_cisd_sc2_selected_do_pt2_end
    ;
    Ezfio.get_cisd_sc2_selected_do_pt2_end ()
  ;;


  let read () = 
    { n_det_max_cisd_sc2  = read_n_det_max_cisd_sc2 ();
      pt2_max             = read_pt2_max ();
      do_pt2_end          = read_do_pt2_end ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
n_det_max_cisd_sc2 = %s
pt2_max            = %s
do_pt2_end         = %s
"
        (Strictly_positive_int.to_string b.n_det_max_cisd_sc2)
        (Positive_float.to_string b.pt2_max)
        (Bool.to_string b.do_pt2_end)
end


