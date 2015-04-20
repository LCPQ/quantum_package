(* =~=~ *)
(* Init *)
(* =~=~ *)

open Qptypes;;
open Qputils;;
open Core.Std;;

module Determinants : sig
(* Generate type *)
   type t = 
     {
       n_det_max_jacobi               : int;
       threshold_generators           : Threshold.t;
       threshold_selectors            : Threshold.t;
       n_states                       : States_number.t;
       s2_eig                         : bool;
       read_wf                        : bool;
       only_single_double_dm          : bool;
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
       n_det_max_jacobi               : int;
       threshold_generators           : Threshold.t;
       threshold_selectors            : Threshold.t;
       n_states                       : States_number.t;
       s2_eig                         : bool;
       read_wf                        : bool;
       only_single_double_dm          : bool;
     } with sexp
   ;;

  let get_default = Qpackage.get_ezfio_default "determinants";;

(* =~=~=~=~=~=~==~=~=~=~=~=~ *)
(* Generate Special Function *)
(* =~=~=~==~=~~=~=~=~=~=~=~=~ *)

(* Read snippet for n_det_max_jacobi *)
  let read_n_det_max_jacobi () =
    if not (Ezfio.has_determinants_n_det_max_jacobi ()) then
       get_default "n_det_max_jacobi"
       |> Int.of_string
       |> Ezfio.set_determinants_n_det_max_jacobi
    ;
    Ezfio.get_determinants_n_det_max_jacobi ()
  ;;
(* Write snippet for n_det_max_jacobi *)
  let write_n_det_max_jacobi =
     Ezfio.set_determinants_n_det_max_jacobi
  ;;

(* Read snippet for n_states *)
  let read_n_states () =
    if not (Ezfio.has_determinants_n_states ()) then
       get_default "n_states"
       |> Int.of_string
       |> Ezfio.set_determinants_n_states
    ;
    Ezfio.get_determinants_n_states ()
      |> States_number.of_int
  ;;
(* Write snippet for n_states *)
  let write_n_states var = 
    States_number.to_int var
    |> Ezfio.set_determinants_n_states
  ;;

(* Read snippet for only_single_double_dm *)
  let read_only_single_double_dm () =
    if not (Ezfio.has_determinants_only_single_double_dm ()) then
       get_default "only_single_double_dm"
       |> Bool.of_string
       |> Ezfio.set_determinants_only_single_double_dm
    ;
    Ezfio.get_determinants_only_single_double_dm ()
  ;;
(* Write snippet for only_single_double_dm *)
  let write_only_single_double_dm =
     Ezfio.set_determinants_only_single_double_dm
  ;;

(* Read snippet for read_wf *)
  let read_read_wf () =
    if not (Ezfio.has_determinants_read_wf ()) then
       get_default "read_wf"
       |> Bool.of_string
       |> Ezfio.set_determinants_read_wf
    ;
    Ezfio.get_determinants_read_wf ()
  ;;
(* Write snippet for read_wf *)
  let write_read_wf =
     Ezfio.set_determinants_read_wf
  ;;

(* Read snippet for s2_eig *)
  let read_s2_eig () =
    if not (Ezfio.has_determinants_s2_eig ()) then
       get_default "s2_eig"
       |> Bool.of_string
       |> Ezfio.set_determinants_s2_eig
    ;
    Ezfio.get_determinants_s2_eig ()
  ;;
(* Write snippet for s2_eig *)
  let write_s2_eig =
     Ezfio.set_determinants_s2_eig
  ;;

(* Read snippet for threshold_generators *)
  let read_threshold_generators () =
    if not (Ezfio.has_determinants_threshold_generators ()) then
       get_default "threshold_generators"
       |> Float.of_string
       |> Ezfio.set_determinants_threshold_generators
    ;
    Ezfio.get_determinants_threshold_generators ()
      |> Threshold.of_float
  ;;
(* Write snippet for threshold_generators *)
  let write_threshold_generators var = 
    Threshold.to_float var
    |> Ezfio.set_determinants_threshold_generators
  ;;

(* Read snippet for threshold_selectors *)
  let read_threshold_selectors () =
    if not (Ezfio.has_determinants_threshold_selectors ()) then
       get_default "threshold_selectors"
       |> Float.of_string
       |> Ezfio.set_determinants_threshold_selectors
    ;
    Ezfio.get_determinants_threshold_selectors ()
      |> Threshold.of_float
  ;;
(* Write snippet for threshold_selectors *)
  let write_threshold_selectors var = 
    Threshold.to_float var
    |> Ezfio.set_determinants_threshold_selectors
  ;;

(* =~=~=~=~=~=~=~=~=~=~=~=~ *)
(* Generate Global Function *)
(* =~=~=~=~=~=~=~=~=~=~=~=~ *)

(* Read all *)
   let read() = 
     Some
     {
       n_det_max_jacobi               = read_n_det_max_jacobi ();
       threshold_generators           = read_threshold_generators ();
       threshold_selectors            = read_threshold_selectors ();
       n_states                       = read_n_states ();
       s2_eig                         = read_s2_eig ();
       read_wf                        = read_read_wf ();
       only_single_double_dm          = read_only_single_double_dm ();
     }
   ;;
(* Write all *)
   let write{ 
              n_det_max_jacobi;
              threshold_generators;
              threshold_selectors;
              n_states;
              s2_eig;
              read_wf;
              only_single_double_dm;
            } =
     write_n_det_max_jacobi               n_det_max_jacobi;
     write_threshold_generators           threshold_generators;
     write_threshold_selectors            threshold_selectors;
     write_n_states                       n_states;
     write_s2_eig                         s2_eig;
     write_read_wf                        read_wf;
     write_only_single_double_dm          only_single_double_dm;
   ;;
(* to_string*)
   let to_string b =
     Printf.sprintf "
   n_det_max_jacobi = %s
   threshold_generators = %s
   threshold_selectors = %s
   n_states = %s
   s2_eig = %s
   read_wf = %s
   only_single_double_dm = %s
   "
       (Int.to_string b.n_det_max_jacobi)
       (Threshold.to_string b.threshold_generators)
       (Threshold.to_string b.threshold_selectors)
       (States_number.to_string b.n_states)
       (Bool.to_string b.s2_eig)
       (Bool.to_string b.read_wf)
       (Bool.to_string b.only_single_double_dm)
   ;;
(* to_rst*)
   let to_rst b =
     Printf.sprintf "
   Maximum number of determinants diagonalized by Jacobi ::
   
     n_det_max_jacobi = %s
   
   Thresholds on generators (fraction of the norm) ::
   
     threshold_generators = %s
   
   Thresholds on selectors (fraction of the norm) ::
   
     threshold_selectors = %s
   
   Number of states to consider ::
   
     n_states = %s
   
   Force the wave function to be an eigenfunction of S^2 ::
   
     s2_eig = %s
   
   If true, read the wave function from the EZFIO file ::
   
     read_wf = %s
   
   If true, The One body DM is calculated with ignoring the Double<->Doubles extra diag elements ::
   
     only_single_double_dm = %s
   
   "
       (Int.to_string b.n_det_max_jacobi)
       (Threshold.to_string b.threshold_generators)
       (Threshold.to_string b.threshold_selectors)
       (States_number.to_string b.n_states)
       (Bool.to_string b.s2_eig)
       (Bool.to_string b.read_wf)
       (Bool.to_string b.only_single_double_dm)
   |> Rst_string.of_string
   ;;
  include Generic_input_of_rst;;
  let of_rst = of_rst t_of_sexp;;

end