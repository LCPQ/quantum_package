(* =~=~ *)
(* Init *)
(* =~=~ *)

open Qptypes;;
open Qputils;;
open Core.Std;;

module Full_ci : sig
(* Generate type *)
   type t = 
     {
       do_pt2_end                     : bool;
       var_pt2_ratio                  : Normalized_float.t;
       n_det_max_fci_property         : Det_number_max.t;
       n_det_max_fci                  : Det_number_max.t;
       pt2_max                        : PT2_energy.t;
     } with sexp
   ;;
  val read  : unit -> t option
  val write : t-> unit
  val to_string : t -> string
  val to_rst : t -> Rst_string.t
  val of_rst : Rst_string.t -> t option
end = struct
(* Generate type *)
   type t = 
     {
       do_pt2_end                     : bool;
       var_pt2_ratio                  : Normalized_float.t;
       n_det_max_fci_property         : Det_number_max.t;
       n_det_max_fci                  : Det_number_max.t;
       pt2_max                        : PT2_energy.t;
     } with sexp
   ;;

  let get_default = Qpackage.get_ezfio_default "full_ci";;

(* =~=~=~=~=~=~==~=~=~=~=~=~ *)
(* Generate Special Function *)
(* =~=~=~==~=~~=~=~=~=~=~=~=~ *)

(* Read snippet for do_pt2_end *)
  let read_do_pt2_end () =
    if not (Ezfio.has_full_ci_do_pt2_end ()) then
       get_default "do_pt2_end"
       |> Bool.of_string
       |> Ezfio.set_full_ci_do_pt2_end
    ;
    Ezfio.get_full_ci_do_pt2_end ()
  ;;
(* Write snippet for do_pt2_end *)
  let write_do_pt2_end =
     Ezfio.set_full_ci_do_pt2_end
  ;;

(* Read snippet for n_det_max_fci *)
  let read_n_det_max_fci () =
    if not (Ezfio.has_full_ci_n_det_max_fci ()) then
       get_default "n_det_max_fci"
       |> Int.of_string
       |> Ezfio.set_full_ci_n_det_max_fci
    ;
    Ezfio.get_full_ci_n_det_max_fci ()
      |> Det_number_max.of_int
  ;;
(* Write snippet for n_det_max_fci *)
  let write_n_det_max_fci var = 
    Det_number_max.to_int var
    |> Ezfio.set_full_ci_n_det_max_fci
  ;;

(* Read snippet for n_det_max_fci_property *)
  let read_n_det_max_fci_property () =
    if not (Ezfio.has_full_ci_n_det_max_fci_property ()) then
       get_default "n_det_max_fci_property"
       |> Int.of_string
       |> Ezfio.set_full_ci_n_det_max_fci_property
    ;
    Ezfio.get_full_ci_n_det_max_fci_property ()
      |> Det_number_max.of_int
  ;;
(* Write snippet for n_det_max_fci_property *)
  let write_n_det_max_fci_property var = 
    Det_number_max.to_int var
    |> Ezfio.set_full_ci_n_det_max_fci_property
  ;;

(* Read snippet for pt2_max *)
  let read_pt2_max () =
    if not (Ezfio.has_full_ci_pt2_max ()) then
       get_default "pt2_max"
       |> Float.of_string
       |> Ezfio.set_full_ci_pt2_max
    ;
    Ezfio.get_full_ci_pt2_max ()
      |> PT2_energy.of_float
  ;;
(* Write snippet for pt2_max *)
  let write_pt2_max var = 
    PT2_energy.to_float var
    |> Ezfio.set_full_ci_pt2_max
  ;;

(* Read snippet for var_pt2_ratio *)
  let read_var_pt2_ratio () =
    if not (Ezfio.has_full_ci_var_pt2_ratio ()) then
       get_default "var_pt2_ratio"
       |> Float.of_string
       |> Ezfio.set_full_ci_var_pt2_ratio
    ;
    Ezfio.get_full_ci_var_pt2_ratio ()
      |> Normalized_float.of_float
  ;;
(* Write snippet for var_pt2_ratio *)
  let write_var_pt2_ratio var = 
    Normalized_float.to_float var
    |> Ezfio.set_full_ci_var_pt2_ratio
  ;;

(* =~=~=~=~=~=~=~=~=~=~=~=~ *)
(* Generate Global Function *)
(* =~=~=~=~=~=~=~=~=~=~=~=~ *)

(* Read all *)
   let read() = 
     Some
     {
       do_pt2_end                     = read_do_pt2_end ();
       var_pt2_ratio                  = read_var_pt2_ratio ();
       n_det_max_fci_property         = read_n_det_max_fci_property ();
       n_det_max_fci                  = read_n_det_max_fci ();
       pt2_max                        = read_pt2_max ();
     }
   ;;
(* Write all *)
   let write{ 
              do_pt2_end;
              var_pt2_ratio;
              n_det_max_fci_property;
              n_det_max_fci;
              pt2_max;
            } =
     write_do_pt2_end                     do_pt2_end;
     write_var_pt2_ratio                  var_pt2_ratio;
     write_n_det_max_fci_property         n_det_max_fci_property;
     write_n_det_max_fci                  n_det_max_fci;
     write_pt2_max                        pt2_max;
   ;;
(* to_string*)
   let to_string b =
     Printf.sprintf "
   do_pt2_end = %s
   var_pt2_ratio = %s
   n_det_max_fci_property = %s
   n_det_max_fci = %s
   pt2_max = %s
   "
       (Bool.to_string b.do_pt2_end)
       (Normalized_float.to_string b.var_pt2_ratio)
       (Det_number_max.to_string b.n_det_max_fci_property)
       (Det_number_max.to_string b.n_det_max_fci)
       (PT2_energy.to_string b.pt2_max)
   ;;
(* to_rst*)
   let to_rst b =
     Printf.sprintf "
   If true, compute the PT2 at the end of the selection ::
   
     do_pt2_end = %s
   
   The selection process stops when the energy ratio variational/(variational+PT2)
is equal to var_pt2_ratio ::
   
     var_pt2_ratio = %s
   
   Max number of determinants in the wave function when you select for a given property ::
   
     n_det_max_fci_property = %s
   
   Max number of determinants in the wave function ::
   
     n_det_max_fci = %s
   
   The selection process stops when the largest PT2 (for all the state) is lower
than pt2_max in absolute value ::
   
     pt2_max = %s
   
   "
       (Bool.to_string b.do_pt2_end)
       (Normalized_float.to_string b.var_pt2_ratio)
       (Det_number_max.to_string b.n_det_max_fci_property)
       (Det_number_max.to_string b.n_det_max_fci)
       (PT2_energy.to_string b.pt2_max)
   |> Rst_string.of_string
   ;;
  include Generic_input_of_rst;;
  let of_rst = of_rst t_of_sexp;;

end