open Qptypes;;
open Qputils;;
open Core.Std;;

module Full_ci : sig
  type t = 
    { n_det_max_fci      : Det_number_max.t;
      pt2_max            : PT2_energy.t;
      do_pt2_end         : bool;
      var_pt2_ratio      : Normalized_float.t;
    } with sexp
  ;;
  val read  : unit -> t option
  val write : t-> unit
  val to_string : t -> string
  val to_rst : t -> Rst_string.t
  val of_rst : Rst_string.t -> t option
end = struct
  type t = 
    { n_det_max_fci      : Det_number_max.t;
      pt2_max            : PT2_energy.t;
      do_pt2_end         : bool;
      var_pt2_ratio      : Normalized_float.t;
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

  let write_n_det_max_fci ndet = 
    Det_number_max.to_int ndet
    |> Ezfio.set_full_ci_n_det_max_fci
  ;;

  let read_var_pt2_ratio () =
    if not (Ezfio.has_full_ci_var_pt2_ratio ()) then
       get_default "var_pt2_ratio"
       |> Float.of_string
       |> Ezfio.set_full_ci_var_pt2_ratio
    ;
    Ezfio.get_full_ci_var_pt2_ratio ()
    |> Normalized_float.of_float
  ;;

  let write_var_pt2_ratio ratio =
    Normalized_float.to_float ratio
    |> Ezfio.set_full_ci_var_pt2_ratio
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

  let write_pt2_max pt2_max = 
    PT2_energy.to_float pt2_max
    |> Ezfio.set_full_ci_pt2_max
  ;;

  let read_do_pt2_end () = 
    if not (Ezfio.has_full_ci_do_pt2_end ()) then
       get_default "do_pt2_end"
       |> Bool.of_string
       |> Ezfio.set_full_ci_do_pt2_end
    ;
    Ezfio.get_full_ci_do_pt2_end ()
  ;;

  let write_do_pt2_end =
    Ezfio.set_full_ci_do_pt2_end
  ;;


  let read () = 
    Some
    { n_det_max_fci = read_n_det_max_fci ();
      pt2_max       = read_pt2_max ();
      do_pt2_end    = read_do_pt2_end ();
      var_pt2_ratio = read_var_pt2_ratio ();
    }
  ;;


  let write { n_det_max_fci ;
              pt2_max       ;
              do_pt2_end    ;
              var_pt2_ratio ;
            } = 
     write_n_det_max_fci n_det_max_fci;
     write_pt2_max       pt2_max;
     write_do_pt2_end    do_pt2_end;
     write_var_pt2_ratio var_pt2_ratio;
  ;;

  let to_string b =
    Printf.sprintf "
n_det_max_fci = %s
pt2_max = %s
do_pt2_end = %s
var_pt2_ratio = %s
"
        (Det_number_max.to_string b.n_det_max_fci)
        (PT2_energy.to_string b.pt2_max)
        (Bool.to_string b.do_pt2_end)
        (Normalized_float.to_string b.var_pt2_ratio)
    ;;

  let to_rst b =
    Printf.sprintf "
Stop when the `n_det` > `n_det_max_fci` ::

  n_det_max_fci = %s

Stop when -E(PT2) < `pt2_max` ::

  pt2_max = %s

Compute E(PT2) at the end ::

  do_pt2_end = %s

Target energy ratio variational/(variational+PT2) ::

  var_pt2_ratio = %s
"
        (Det_number_max.to_string b.n_det_max_fci)
        (PT2_energy.to_string b.pt2_max)
        (Bool.to_string b.do_pt2_end)
        (Normalized_float.to_string b.var_pt2_ratio)
    |> Rst_string.of_string
    ;;

  include Generic_input_of_rst;;
  let of_rst = of_rst t_of_sexp;;


end


