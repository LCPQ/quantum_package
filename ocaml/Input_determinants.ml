open Qptypes;;
open Qputils;;
open Core.Std;;

module Determinants : sig
  type t = 
    { n_int                  : N_int_number.t;
      bit_kind               : Bit_kind.t;
      mo_label               : MO_label.t;
      n_det                  : Det_number.t;
      n_states               : States_number.t;
      n_states_diag          : States_number.t;
      n_det_max_jacobi       : Strictly_positive_int.t;
      threshold_generators   : Threshold.t;
      threshold_selectors    : Threshold.t; 
      read_wf                : bool;
      expected_s2            : Positive_float.t;
      s2_eig                 : bool;
      psi_coef               : Det_coef.t array;
      psi_det                : Determinant.t array;
    } with sexp
  val read  : unit -> t
  val write : t -> unit
  val to_string : t -> string
  val to_rst : t -> Rst_string.t
  val of_rst : Rst_string.t -> t option
end = struct
  type t = 
    { n_int                  : N_int_number.t;
      bit_kind               : Bit_kind.t;
      mo_label               : MO_label.t;
      n_det                  : Det_number.t;
      n_states               : States_number.t;
      n_states_diag          : States_number.t;
      n_det_max_jacobi       : Strictly_positive_int.t;
      threshold_generators   : Threshold.t;
      threshold_selectors    : Threshold.t; 
      read_wf                : bool;
      expected_s2            : Positive_float.t;
      s2_eig                 : bool;
      psi_coef               : Det_coef.t array;
      psi_det                : Determinant.t array;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "determinants";;

  let read_n_int () =
    if not (Ezfio.has_determinants_n_int()) then
       Ezfio.get_mo_basis_mo_tot_num ()
       |> Bitlist.n_int_of_mo_tot_num
       |> N_int_number.to_int
       |> Ezfio.set_determinants_n_int
    ;
    Ezfio.get_determinants_n_int ()
    |> N_int_number.of_int
  ;;

  let write_n_int n =
    N_int_number.to_int n
    |> Ezfio.set_determinants_n_int
  ;;


  let read_bit_kind () =
    if not (Ezfio.has_determinants_bit_kind ()) then
      Lazy.force Qpackage.bit_kind
      |> Bit_kind.to_int
      |> Ezfio.set_determinants_bit_kind
    ;
    Ezfio.get_determinants_bit_kind ()
    |> Bit_kind.of_int
  ;;

  let write_bit_kind b =
    Bit_kind.to_int b
    |> Ezfio.set_determinants_bit_kind
  ;;


  let read_mo_label () =
    if (not (Ezfio.has_determinants_mo_label ())) then
      Ezfio.get_mo_basis_mo_label ()
      |> Ezfio.set_determinants_mo_label 
      ;
    Ezfio.get_determinants_mo_label ()
    |> MO_label.of_string
  ;;

  let write_mo_label l =
    MO_label.to_string l
    |> Ezfio.set_determinants_mo_label
  ;;


  let read_n_det () =
    if not (Ezfio.has_determinants_n_det ()) then
      Ezfio.set_determinants_n_det 1
    ;
    Ezfio.get_determinants_n_det ()
    |> Det_number.of_int
  ;;

  let write_n_det n =
    Det_number.to_int n
    |> Ezfio.set_determinants_n_det
  ;;


  let read_n_states () =
    if not (Ezfio.has_determinants_n_states ()) then
      Ezfio.set_determinants_n_states 1
    ;
    Ezfio.get_determinants_n_states ()
    |> States_number.of_int
  ;;

  let write_n_states n =
    States_number.to_int n
    |> Ezfio.set_determinants_n_states
  ;;


  let read_n_states_diag () =
    if not (Ezfio.has_determinants_n_states_diag ()) then
      read_n_states ()
      |> States_number.to_int
      |> Ezfio.set_determinants_n_states_diag 
    ;
    Ezfio.get_determinants_n_states_diag ()
    |> States_number.of_int
  ;;

  let write_n_states_diag n =
    States_number.to_int n
    |> Ezfio.set_determinants_n_states_diag
  ;;


  let read_n_det_max_jacobi () =
    if not (Ezfio.has_determinants_n_det_max_jacobi ()) then
      get_default "n_det_max_jacobi"
      |> Int.of_string
      |> Ezfio.set_determinants_n_det_max_jacobi 
    ;
    Ezfio.get_determinants_n_det_max_jacobi ()
    |> Strictly_positive_int.of_int
  ;;

  let write_n_det_max_jacobi n =
    Strictly_positive_int.to_int n
    |> Ezfio.set_determinants_n_det_max_jacobi
  ;;


  let read_threshold_generators () =
    if not (Ezfio.has_determinants_threshold_generators ()) then
      get_default "threshold_generators"
      |> Float.of_string
      |> Ezfio.set_determinants_threshold_generators
    ;
    Ezfio.get_determinants_threshold_generators ()
    |> Threshold.of_float
  ;;

  let write_threshold_generators t =
    Threshold.to_float t
    |> Ezfio.set_determinants_threshold_generators
  ;;


  let read_threshold_selectors () =
    if not (Ezfio.has_determinants_threshold_selectors ()) then
      get_default "threshold_selectors"
      |> Float.of_string
      |> Ezfio.set_determinants_threshold_selectors
    ;
    Ezfio.get_determinants_threshold_selectors ()
    |> Threshold.of_float
  ;;

  let write_threshold_selectors t =
    Threshold.to_float t
    |> Ezfio.set_determinants_threshold_selectors
  ;;


  let read_read_wf () =
    if not (Ezfio.has_determinants_read_wf ()) then
      get_default "read_wf"
      |> Bool.of_string
      |> Ezfio.set_determinants_read_wf 
    ;
    Ezfio.get_determinants_read_wf ()
  ;;

  let write_read_wf = Ezfio.set_determinants_read_wf ;;
    

  let read_expected_s2 () =
    if not (Ezfio.has_determinants_expected_s2 ()) then
      begin
        let na = Ezfio.get_electrons_elec_alpha_num ()
        and nb = Ezfio.get_electrons_elec_beta_num  ()
        in
        let s = 0.5 *. (Float.of_int (na - nb))
        in
        Ezfio.set_determinants_expected_s2 ( s *. (s +. 1.) )
      end
    ;
    Ezfio.get_determinants_expected_s2 ()
    |> Positive_float.of_float
  ;;

  let write_expected_s2 s2 =
    Positive_float.to_float s2
    |> Ezfio.set_determinants_expected_s2 
  ;;


  let read_s2_eig () =
    if not (Ezfio.has_determinants_s2_eig ()) then
      get_default "s2_eig"
      |> Bool.of_string
      |> Ezfio.set_determinants_s2_eig
    ;
    Ezfio.get_determinants_s2_eig ()
  ;;

  let write_s2_eig = Ezfio.set_determinants_s2_eig ;;


  let read_psi_coef () =
    if not (Ezfio.has_determinants_psi_coef ()) then
        Ezfio.ezfio_array_of_list ~rank:1 ~dim:[| 1 |] ~data:[1.]
        |> Ezfio.set_determinants_psi_coef 
      ;
    Ezfio.get_determinants_psi_coef ()
    |> Ezfio.flattened_ezfio
    |> Array.map ~f:Det_coef.of_float
  ;;

  let write_psi_coef ~n_det c =
    let n_det = Det_number.to_int n_det
    and c = Array.to_list c
            |> List.map ~f:Det_coef.to_float
    in
    Ezfio.ezfio_array_of_list ~rank:1 ~dim:[| n_det |] ~data:c
    |> Ezfio.set_determinants_psi_coef 
  ;;


  let read_psi_det () =
    let n_int = read_n_int () 
    and n_alpha = Ezfio.get_electrons_elec_alpha_num ()
        |> Elec_alpha_number.of_int 
    and n_beta = Ezfio.get_electrons_elec_beta_num ()
        |> Elec_beta_number.of_int 
    in
    if not (Ezfio.has_determinants_psi_det ()) then
      begin
        let mo_tot_num = MO_number.get_max () in
        let rec build_data accu =  function
          | 0 -> accu
          | n -> build_data ((MO_number.of_int ~max:mo_tot_num n)::accu) (n-1)
        in
        let det_a = build_data [] (Elec_alpha_number.to_int n_alpha)
          |> Bitlist.of_mo_number_list n_int
        and det_b = build_data [] (Elec_beta_number.to_int n_beta)
          |> Bitlist.of_mo_number_list n_int
        in
        let data = ( (Bitlist.to_int64_list det_a) @ 
          (Bitlist.to_int64_list det_b) ) 
        in
        Ezfio.ezfio_array_of_list ~rank:3 ~dim:[| N_int_number.to_int n_int ; 2 ; 1 |] ~data:data
          |> Ezfio.set_determinants_psi_det ;
      end  ;
    let n_int = N_int_number.to_int n_int in
    let psi_det_array = Ezfio.get_determinants_psi_det () in
    let dim = psi_det_array.Ezfio.dim
    and data =  Ezfio.flattened_ezfio psi_det_array
    in
    assert (n_int = dim.(0));
    assert (dim.(1) = 2);
    assert (dim.(2) = (Det_number.to_int (read_n_det ())));
    List.init dim.(2) ~f:(fun i ->
      Array.sub ~pos:(2*n_int*i) ~len:(2*n_int) data)
    |> List.map ~f:(Determinant.of_int64_array
      ~n_int:(N_int_number.of_int n_int)
      ~alpha:n_alpha ~beta:n_beta )
    |> Array.of_list
  ;;

  let write_psi_det ~n_int ~n_det d =
    let data = Array.to_list d
      |> Array.concat 
      |> Array.to_list
    in
    Ezfio.ezfio_array_of_list ~rank:3 ~dim:[| N_int_number.to_int n_int ; 2 ; Det_number.to_int n_det |] ~data:data
    |> Ezfio.set_determinants_psi_det
  ;;


  let read () =
    { n_int                  = read_n_int ()                ;
      bit_kind               = read_bit_kind ()             ;
      mo_label               = read_mo_label ()             ;
      n_det                  = read_n_det ()                ;
      n_states               = read_n_states ()             ;
      n_states_diag          = read_n_states_diag ()        ;
      n_det_max_jacobi       = read_n_det_max_jacobi ()     ;
      threshold_generators   = read_threshold_generators () ;
      threshold_selectors    = read_threshold_selectors ()  ;
      read_wf                = read_read_wf ()              ;
      expected_s2            = read_expected_s2 ()          ;
      s2_eig                 = read_s2_eig ()               ;
      psi_coef               = read_psi_coef ()             ;
      psi_det                = read_psi_det ()              ;
    }
  ;;

  let write { n_int                ;
              bit_kind             ;
              mo_label             ;
              n_det                ;
              n_states             ;
              n_states_diag        ;
              n_det_max_jacobi     ;
              threshold_generators ;
              threshold_selectors  ;
              read_wf              ;
              expected_s2          ;
              s2_eig               ;
              psi_coef             ;
              psi_det              ;
            } =
     write_n_int n_int ;
     write_bit_kind bit_kind;
     write_mo_label mo_label;
     write_n_det n_det;
     write_n_states n_states;
     write_n_states_diag n_states_diag;
     write_n_det_max_jacobi n_det_max_jacobi;
     write_threshold_generators threshold_generators;
     write_threshold_selectors threshold_selectors;
     write_read_wf read_wf;
     write_expected_s2 expected_s2;
     write_s2_eig s2_eig;
     write_psi_coef ~n_det:n_det psi_coef;
     write_psi_det ~n_int:n_int ~n_det:n_det psi_det;
  ;;


  let to_rst b =
    let mo_tot_num = Ezfio.get_mo_basis_mo_tot_num () in
    let mo_tot_num = MO_number.of_int mo_tot_num ~max:mo_tot_num in
    let det_text = 
      List.map2_exn ~f:(fun coef det ->
        Printf.sprintf "  %F\n%s\n"
        (Det_coef.to_float coef)
        (Determinant.to_string ~mo_tot_num:mo_tot_num det 
         |> String.split ~on:'\n'
         |> List.map ~f:(fun x -> "  "^x)
         |> String.concat ~sep:"\n"
        )
      ) (Array.to_list b.psi_coef) (Array.to_list b.psi_det)
      |> String.concat ~sep:"\n"
    in
    Printf.sprintf "
Read the current wave function ::

  read_wf = %s

Label of the MOs on which the determinants were computed ::

  mo_label = %s

Force the selected wave function to be an eigenfunction of S^2.
If true, input the expected value of S^2 ::

  s2_eig      = %s
  expected_s2 = %s

Thresholds on generators and selectors (fraction of the norm) ::

  threshold_generators = %s
  threshold_selectors  = %s

Number of requested states, and number of states used for the
Davidson diagonalization ::

  n_states      = %s
  n_states_diag = %s

Maximum size of the Hamiltonian matrix that will be fully diagonalized ::

  n_det_max_jacobi = %s

Number of determinants ::

  n_det = %s

Determinants ::

%s
"
     (b.read_wf       |> Bool.to_string)
     (b.mo_label      |> MO_label.to_string)
     (b.s2_eig        |> Bool.to_string)
     (b.expected_s2   |> Positive_float.to_string)
     (b.threshold_generators |> Threshold.to_string)
     (b.threshold_selectors |> Threshold.to_string)
     (b.n_states      |> States_number.to_string)
     (b.n_states_diag |> States_number.to_string)
     (b.n_det_max_jacobi |> Strictly_positive_int.to_string)
     (b.n_det         |> Det_number.to_string)
     det_text
     |> Rst_string.of_string
  ;;

  let to_string b =
    let mo_tot_num = Ezfio.get_mo_basis_mo_tot_num () in
    let mo_tot_num = MO_number.of_int mo_tot_num ~max:mo_tot_num in
    Printf.sprintf "
n_int                  = %s
bit_kind               = %s
mo_label               = \"%s\"
n_det                  = %s
n_states               = %s
n_states_diag          = %s
n_det_max_jacobi       = %s
threshold_generators   = %s
threshold_selectors    = %s
read_wf                = %s
expected_s2            = %s
s2_eig                 = %s
psi_coef               = %s
psi_det                = %s
"
     (b.n_int         |> N_int_number.to_string)
     (b.bit_kind      |> Bit_kind.to_string)
     (b.mo_label      |> MO_label.to_string)
     (b.n_det         |> Det_number.to_string)
     (b.n_states      |> States_number.to_string)
     (b.n_states_diag |> States_number.to_string)
     (b.n_det_max_jacobi |> Strictly_positive_int.to_string)
     (b.threshold_generators |> Threshold.to_string)
     (b.threshold_selectors |> Threshold.to_string)
     (b.read_wf       |> Bool.to_string)
     (b.expected_s2   |> Positive_float.to_string)
     (b.s2_eig        |> Bool.to_string)
     (b.psi_coef  |> Array.to_list |> List.map ~f:Det_coef.to_string
      |> String.concat ~sep:", ")
     (b.psi_det   |> Array.to_list |> List.map ~f:(Determinant.to_string
       ~mo_tot_num:mo_tot_num) |> String.concat ~sep:"\n\n")
  ;;

  let of_rst r =
    let r = Rst_string.to_string r
    in

    (* Split into header and determinants data *)
    let idx = String.substr_index_exn r ~pos:0 ~pattern:"\nDeterminants"
    in
    let (header, dets) = 
       (String.prefix r idx, String.suffix r ((String.length r)-idx) )
    in

    (* Handle header *)
    let header = r
    |> String.split ~on:'\n'
    |> List.filter ~f:(fun line ->
        if (line = "") then
          false
        else
          ( (String.contains line '=') && (line.[0] = ' ') )
       )
    |> List.map ~f:(fun line ->
        "("^(
        String.tr line ~target:'=' ~replacement:' '
        |> String.strip
        )^")" )
    |> String.concat 
    in

    (* Handle determinant coefs *)
    let dets = match ( dets
    |> String.split ~on:'\n'
    |> List.map ~f:(String.strip)
    ) with 
    | _::lines -> lines 
    | _ -> failwith "Error in determinants"
    in

    let psi_coef = 
      let rec read_coefs accu = function
      | [] -> List.rev accu
      | ""::c::tail -> 
          read_coefs (c::accu) tail
      | _::tail -> read_coefs accu tail
      in
      let a = read_coefs [] dets
      |> String.concat ~sep:" "
      in
      "(psi_coef ("^a^"))"
    in

    (* Handle determinants *)
    let psi_det = 
      let n_alpha = Ezfio.get_electrons_elec_alpha_num ()
        |> Elec_alpha_number.of_int 
      and n_beta = Ezfio.get_electrons_elec_beta_num ()
        |> Elec_beta_number.of_int 
      in
      let rec read_dets accu = function
      | [] -> List.rev accu
      | ""::c::alpha::beta::tail -> 
          begin
            let alpha = String.rev alpha |> Bitlist.of_string ~zero:'-' ~one:'+'
            and beta  = String.rev beta  |> Bitlist.of_string ~zero:'-' ~one:'+'
            in
            let newdet = Determinant.of_bitlist_couple 
              ~alpha:n_alpha ~beta:n_beta (alpha,beta) 
            |> Determinant.sexp_of_t |> Sexplib.Sexp.to_string
            in
            read_dets (newdet::accu) tail
          end
      | _::tail -> read_dets accu tail
      in
      let a = read_dets [] dets
      |> String.concat 
      in
      "(psi_det ("^a^"))"
    in

    let bitkind = Printf.sprintf "(bit_kind %d)" (Lazy.force Qpackage.bit_kind
      |> Bit_kind.to_int)
    and n_int = Printf.sprintf "(n_int %d)" (N_int_number.get_max ()) in
    let s = String.concat [ header ; bitkind ; n_int ; psi_coef ; psi_det]
    in

    Generic_input_of_rst.evaluate_sexp t_of_sexp s
  ;;

end


