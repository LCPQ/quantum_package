open Qptypes;;
open Qputils;;
open Core.Std;;

module Full_ci : sig
  type t = 
    { n_det_max_fci      : Det_number_max.t;
      pt2_max            : PT2_energy.t;
      do_pt2_end         : bool;
    } with sexp
  ;;
  val read : unit -> t
  val to_string : t -> string
  val to_rst : t -> Rst_string.t
  val of_rst : Rst_string.t -> t
end = struct
  type t = 
    { n_det_max_fci      : Det_number_max.t;
      pt2_max            : PT2_energy.t;
      do_pt2_end         : bool;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "full_ci";;

  let read_n_det_max_fci () = 
    if not (Ezfio.has_full_ci_n_det_max_fci ()) then
       get_default "n_det_max_fci"
       |> Int.of_string
       |> Ezfio.set_full_ci_n_det_max_fci
    ;
    Ezfio.get_full_ci_n_det_max_fci ()
    |> Det_number_max.of_int
  ;;

  let read_pt2_max () = 
    if not (Ezfio.has_full_ci_pt2_max ()) then
       get_default "pt2_max"
       |> Float.of_string
       |> Ezfio.set_full_ci_pt2_max
    ;
    Ezfio.get_full_ci_pt2_max ()
    |> PT2_energy.of_float
  ;;

  let read_do_pt2_end () = 
    if not (Ezfio.has_full_ci_do_pt2_end ()) then
       get_default "do_pt2_end"
       |> Bool.of_string
       |> Ezfio.set_full_ci_do_pt2_end
    ;
    Ezfio.get_full_ci_do_pt2_end ()
  ;;


  let read () = 
    { n_det_max_fci    = read_n_det_max_fci ();
    pt2_max            = read_pt2_max ();
    do_pt2_end         = read_do_pt2_end ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
n_det_max_fci = %s
pt2_max = %s
do_pt2_end = %s
"
        (Det_number_max.to_string b.n_det_max_fci)
        (PT2_energy.to_string b.pt2_max)
        (Bool.to_string b.do_pt2_end)
    ;;

  let to_rst b =
    Printf.sprintf "
Stop when the `n_det` > `n_det_max_fci` ::

  n_det_max_fci = %s

Stop when -E(PT2) < `pt2_max` ::

  pt2_max = %s

Compute E(PT2) at the end ::

  do_pt2_end = %s

"
        (Det_number_max.to_string b.n_det_max_fci)
        (PT2_energy.to_string b.pt2_max)
        (Bool.to_string b.do_pt2_end)
    |> Rst_string.of_string
    ;;

  let of_rst s =
    let s = Rst_string.to_string s
    |> String.split ~on:'\n'
    |> List.filter ~f:(fun line ->
        String.contains line '=')
    |> List.map ~f:(fun line ->
        "("^(
        String.tr line ~target:'=' ~replacement:' '
        )^")" )
    |> String.concat
    in
    Sexp.of_string ("("^s^")")
    |> t_of_sexp
  ;;


end


