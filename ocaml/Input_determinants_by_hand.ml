open Qptypes;;
open Qputils;;
open Core.Std;;

module Determinants_by_hand : sig
  type t = 
    { n_int                  : N_int_number.t;
      bit_kind               : Bit_kind.t;
      n_det                  : Det_number.t;
      expected_s2            : Positive_float.t;
      psi_coef               : Det_coef.t array;
      psi_det                : Determinant.t array;
    } with sexp
  val read  : unit -> t option
  val write : t -> unit
  val to_string : t -> string
  val to_rst : t -> Rst_string.t
  val of_rst : Rst_string.t -> t option
end = struct
  type t = 
    { n_int                  : N_int_number.t;
      bit_kind               : Bit_kind.t;
      n_det                  : Det_number.t;
      expected_s2            : Positive_float.t;
      psi_coef               : Det_coef.t array;
      psi_det                : Determinant.t array;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "determinants";;

  let n_det_read_max = 10_000 ;;

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

  let write_n_states_diag ~n_states n =
    let n_states = States_number.to_int n_states
    and n = States_number.to_int n
    in
    Ezfio.set_determinants_n_states_diag (max n_states n)
  ;;

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

  let read_psi_coef () =
    if not (Ezfio.has_determinants_psi_coef ()) then
      begin
        let n_states =
          read_n_states ()
          |> States_number.to_int
        in
        Ezfio.ezfio_array_of_list ~rank:2 ~dim:[| 1 ; n_states |] 
          ~data:(List.init n_states ~f:(fun i -> if (i=0) then 1. else 0. ))
          |> Ezfio.set_determinants_psi_coef 
      end;
    Ezfio.get_determinants_psi_coef ()
    |> Ezfio.flattened_ezfio
    |> Array.map ~f:Det_coef.of_float
  ;;

  let write_psi_coef ~n_det c =
    let n_det = Det_number.to_int n_det
    and c = Array.to_list c
            |> List.map ~f:Det_coef.to_float
    and n_states = 
      read_n_states () |> States_number.to_int
    in
    Ezfio.ezfio_array_of_list ~rank:2 ~dim:[| n_det ; n_states |] ~data:c
    |> Ezfio.set_determinants_psi_coef 
  ;;


  let read_psi_det () =
      let n_int = 
        read_n_int () 
      and alpha =
        Ezfio.get_electrons_elec_alpha_num ()
        |> Elec_alpha_number.of_int 
      and beta =
        Ezfio.get_electrons_elec_beta_num ()
        |> Elec_beta_number.of_int 
      in
      if not (Ezfio.has_determinants_psi_det ()) then
        begin
            let mo_tot_num =
                MO_number.get_max ()
            in
            let rec build_data accu =  function
              | 0 -> accu
              | n -> build_data ((MO_number.of_int ~max:mo_tot_num n)::accu) (n-1)
            in
            let det_a =
                build_data [] (Elec_alpha_number.to_int alpha)
                |> Bitlist.of_mo_number_list n_int
            and det_b =
                build_data [] (Elec_beta_number.to_int beta)
                |> Bitlist.of_mo_number_list n_int
            in
            let data =
              ( (Bitlist.to_int64_list det_a) @ 
                (Bitlist.to_int64_list det_b) ) 
            in
            Ezfio.ezfio_array_of_list ~rank:3 ~dim:[| N_int_number.to_int n_int ; 2 ; 1 |] ~data:data
              |> Ezfio.set_determinants_psi_det ;
        end  ;
      let n_int_i =
          N_int_number.to_int n_int in
      let psi_det_array =
          Ezfio.get_determinants_psi_det ()
      in
      let dim =
          psi_det_array.Ezfio.dim
      and data = 
          Ezfio.flattened_ezfio psi_det_array
      in
      assert (n_int_i = dim.(0));
      assert (dim.(1) = 2);
      assert (dim.(2) = (Det_number.to_int (read_n_det ())));
      let len = 
        2 * n_int_i
      in
      Array.init dim.(2) ~f:(fun i ->
         Array.sub ~pos:(len * i) ~len data
         |> Determinant.of_int64_array ~n_int ~alpha ~beta 
      )
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
    if (Ezfio.has_mo_basis_mo_tot_num ()) then
      let n_det = 
         read_n_det ()
      in
      if ( (Det_number.to_int n_det) > n_det_read_max ) then
        None
      else
        Some
        { n_int                  = read_n_int ()                ;
          bit_kind               = read_bit_kind ()             ;
          n_det                  = read_n_det ()                ;
          expected_s2            = read_expected_s2 ()          ;
          psi_coef               = read_psi_coef ()             ;
          psi_det                = read_psi_det ()              ;
        }
    else
      None
  ;;

  let write { n_int                ;
              bit_kind             ;
              n_det                ;
              expected_s2          ;
              psi_coef             ;
              psi_det              ;
            } =
     write_n_int n_int ;
     write_bit_kind bit_kind;
     write_n_det n_det;
     write_expected_s2 expected_s2;
     write_psi_coef ~n_det:n_det psi_coef ;
     write_psi_det ~n_int:n_int ~n_det:n_det psi_det;
  ;;


  let to_rst b =
    let max =
      Ezfio.get_mo_basis_mo_tot_num () 
    in
    let mo_tot_num =
      MO_number.of_int ~max max
    in
    let det_text = 
      let nstates =
        read_n_states ()
        |> States_number.to_int
      and ndet =
        Det_number.to_int b.n_det
      in
      let coefs_string i =
        Array.init nstates (fun j -> 
          let ishift = 
            j*ndet
          in
          if (ishift < Array.length b.psi_coef) then
            b.psi_coef.(i+ishift)
            |> Det_coef.to_float 
            |> Float.to_string
          else
            "0."
        )
        |> String.concat_array ~sep:"\t"
      in
      Array.init ndet ~f:(fun i ->
        String.concat [ "  " ; 
          (coefs_string i) ; "\n" ; 
          (Determinant.to_string ~mo_tot_num b.psi_det.(i)) ; "\n" ]
      )
      |> String.concat_array ~sep:"\n"
    in
    Printf.sprintf "
Force the selected wave function to be an eigenfunction of S^2.
If true, input the expected value of S^2 ::

  expected_s2 = %s

Number of determinants ::

  n_det = %s

Determinants ::

%s
"
     (b.expected_s2   |> Positive_float.to_string)
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
n_det                  = %s
expected_s2            = %s
psi_coef               = %s
psi_det                = %s
"
     (b.n_int         |> N_int_number.to_string)
     (b.bit_kind      |> Bit_kind.to_string)
     (b.n_det         |> Det_number.to_string)
     (b.expected_s2   |> Positive_float.to_string)
     (b.psi_coef  |> Array.to_list |> List.map ~f:Det_coef.to_string
      |> String.concat ~sep:", ")
     (b.psi_det   |> Array.to_list |> List.map ~f:(Determinant.to_string
       ~mo_tot_num:mo_tot_num) |> String.concat ~sep:"\n\n")
  ;;

  let of_rst r =
    let dets = Rst_string.to_string r
    in

    (* Split into header and determinants data *)
    let idx = String.substr_index_exn dets ~pos:0 ~pattern:"\nDeterminants"
    in
    let header = 
       String.prefix dets idx
    in

    (* Handle header *)
    let header = header
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

    (* Handle determinants and coefs *)
    let dets_stream = 

      let ipos, jmax =
        ref idx,  String.length dets
      in
      let next_line =
        Stream.from (fun _ ->
          let rec loop line =
            let j = 
              !ipos + 1
            in
            ipos := j;
            if (j < jmax) then
              match dets.[j] with
              | '\n' -> Some (String.of_char_list @@ List.rev line )
              | ' '  -> loop line 
              | c    -> loop (c :: line) 
            else
              None
          in loop []
        )
      in
      ignore @@ Stream.next next_line; (* Determinants :: *)
      ignore @@ Stream.next next_line; (*                 *)
      Stream.from (fun _ ->
          try
            begin
              let result =
                let coefs = 
                  let line = 
                     Stream.next next_line
                  in
                  String.split ~on:'\t' line
                  |> Array.of_list
                  |> Array.map ~f:(fun x -> Det_coef.of_float @@ Float.of_string x)
                in
                Some (coefs,
                      Stream.next next_line |> String.rev,
                      Stream.next next_line |> String.rev )
              in
              ignore @@ Stream.next next_line;
              result
            end
          with
          | Stream.Failure -> None
      )
    in
      
 
        
    let psi_coef, psi_det = 

      let alpha =
        Ezfio.get_electrons_elec_alpha_num ()
        |> Elec_alpha_number.of_int 
      and beta = 
        Ezfio.get_electrons_elec_beta_num ()
        |> Elec_beta_number.of_int 
      and n_int =
        N_int_number.get_max ()
        |> N_int_number.of_int
      in

      let rec read coefs dets_bit = function
        | None -> (List.rev coefs), (List.rev dets_bit)
        | Some (c, alpha_str, beta_str) -> 
            begin
                ignore @@ Stream.next dets_stream;
                let new_coefs = 
                   c :: coefs
                and new_dets =
                  let newdet =
                    (Bitlist.of_string_mp alpha_str, Bitlist.of_string_mp beta_str)
                    |> Determinant.of_bitlist_couple ~n_int ~alpha ~beta
                  in
                  newdet :: dets_bit
                in
                read new_coefs new_dets (Stream.peek dets_stream)
            end
      in
        
      let coefs, dets_bit = 
          read [] [] (Stream.peek dets_stream)
      in
      let nstates =
          List.hd_exn coefs |> Array.length
      in
      let a =
          let extract_state i = 
            let i = 
              i-1
            in
            List.map ~f:(fun x -> x.(i)) coefs
          in
          let rec build_result accu = function
          | 0 -> accu
          | i -> 
              let new_accu =
                (extract_state i) :: accu
              in
              build_result new_accu (i-1)
          in
          build_result [] nstates 
      in
      let new_coefs =
          List.concat a |> Array.of_list
      and new_dets = 
          Array.of_list dets_bit
      in
      new_coefs, new_dets
    in


    let bitkind = 
      Printf.sprintf "(bit_kind %d)" (Lazy.force Qpackage.bit_kind
      |> Bit_kind.to_int)
    and n_int =
      Printf.sprintf "(n_int %d)" (N_int_number.get_max ())
    in
    let s =
      [ header ; bitkind ; n_int ; "(psi_coef ())" ; "(psi_det ())"]
      |> String.concat 
    in
    let result = 
      Generic_input_of_rst.evaluate_sexp t_of_sexp s
    in
    match result with
    | Some x -> Some { x with psi_coef ; psi_det }
    | None -> None
  ;;

end


