(* =~=~ *)
(* Init *)
(* =~=~ *)

open Qptypes;;
open Qputils;;
open Core;;

module Ao_basis : sig
(* Generate type *)
   type t = 
     {
       threshold_overlap_ao_eigenvalues : Threshold.t;
     } [@@deriving sexp]
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
       threshold_overlap_ao_eigenvalues : Threshold.t;
     } [@@deriving sexp]
   ;;

  let get_default = Qpackage.get_ezfio_default "ao_basis";;

(* =~=~=~=~=~=~==~=~=~=~=~=~ *)
(* Generate Special Function *)
(* =~=~=~==~=~~=~=~=~=~=~=~=~ *)

(* Read snippet for ao_cartesian *)
  let read_ao_cartesian () =
    if not (Ezfio.has_ao_basis_ao_cartesian ()) then
       get_default "ao_cartesian"
       |> Bool.of_string
       |> Ezfio.set_ao_basis_ao_cartesian
    ;
    Ezfio.get_ao_basis_ao_cartesian ()
  ;;
(* Write snippet for ao_cartesian *)
  let write_ao_cartesian =
     Ezfio.set_ao_basis_ao_cartesian
  ;;

(* Read snippet for ao_prim_num_max *)
  let read_ao_prim_num_max () =
    if not (Ezfio.has_ao_basis_ao_prim_num_max ()) then
       get_default "ao_prim_num_max"
       |> Int.of_string
       |> Ezfio.set_ao_basis_ao_prim_num_max
    ;
    Ezfio.get_ao_basis_ao_prim_num_max ()
  ;;
(* Write snippet for ao_prim_num_max *)
  let write_ao_prim_num_max =
     Ezfio.set_ao_basis_ao_prim_num_max
  ;;

(* Read snippet for integral_kinetic *)
  let read_integral_kinetic () =
    if not (Ezfio.has_ao_basis_integral_kinetic ()) then
       get_default "integral_kinetic"
       |> Float.of_string
       |> Ezfio.set_ao_basis_integral_kinetic
    ;
    Ezfio.get_ao_basis_integral_kinetic ()
  ;;
(* Write snippet for integral_kinetic *)
  let write_integral_kinetic =
     Ezfio.set_ao_basis_integral_kinetic
  ;;

(* Read snippet for integral_nuclear *)
  let read_integral_nuclear () =
    if not (Ezfio.has_ao_basis_integral_nuclear ()) then
       get_default "integral_nuclear"
       |> Float.of_string
       |> Ezfio.set_ao_basis_integral_nuclear
    ;
    Ezfio.get_ao_basis_integral_nuclear ()
  ;;
(* Write snippet for integral_nuclear *)
  let write_integral_nuclear =
     Ezfio.set_ao_basis_integral_nuclear
  ;;

(* Read snippet for integral_overlap *)
  let read_integral_overlap () =
    if not (Ezfio.has_ao_basis_integral_overlap ()) then
       get_default "integral_overlap"
       |> Float.of_string
       |> Ezfio.set_ao_basis_integral_overlap
    ;
    Ezfio.get_ao_basis_integral_overlap ()
  ;;
(* Write snippet for integral_overlap *)
  let write_integral_overlap =
     Ezfio.set_ao_basis_integral_overlap
  ;;

(* Read snippet for threshold_overlap_ao_eigenvalues *)
  let read_threshold_overlap_ao_eigenvalues () =
    if not (Ezfio.has_ao_basis_threshold_overlap_ao_eigenvalues ()) then
       get_default "threshold_overlap_ao_eigenvalues"
       |> Float.of_string
       |> Ezfio.set_ao_basis_threshold_overlap_ao_eigenvalues
    ;
    Ezfio.get_ao_basis_threshold_overlap_ao_eigenvalues ()
      |> Threshold.of_float
  ;;
(* Write snippet for threshold_overlap_ao_eigenvalues *)
  let write_threshold_overlap_ao_eigenvalues var = 
    Threshold.to_float var
    |> Ezfio.set_ao_basis_threshold_overlap_ao_eigenvalues
  ;;

(* =~=~=~=~=~=~=~=~=~=~=~=~ *)
(* Generate Global Function *)
(* =~=~=~=~=~=~=~=~=~=~=~=~ *)

(* Read all *)
   let read() = 
     Some
     {
       threshold_overlap_ao_eigenvalues = read_threshold_overlap_ao_eigenvalues ();
     }
   ;;
(* Write all *)
   let write{ 
              threshold_overlap_ao_eigenvalues;
            } =
     write_threshold_overlap_ao_eigenvalues threshold_overlap_ao_eigenvalues;
   ;;
(* to_string*)
   let to_string b =
     Printf.sprintf "
   threshold_overlap_ao_eigenvalues = %s
   "
       (Threshold.to_string b.threshold_overlap_ao_eigenvalues)
   ;;
(* to_rst*)
   let to_rst b =
     Printf.sprintf "
   Threshold on the magnitude of the smallest eigenvalues of the overlap matrix in the AO basis ::
   
     threshold_overlap_ao_eigenvalues = %s
   
   "
       (Threshold.to_string b.threshold_overlap_ao_eigenvalues)
   |> Rst_string.of_string
   ;;
  include Generic_input_of_rst;;
  let of_rst = of_rst t_of_sexp;;

end