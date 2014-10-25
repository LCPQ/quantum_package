open Qptypes;;
open Qputils;;
open Core.Std;;

module Mo_basis : sig
  type t = 
    { mo_tot_num      : MO_number.t ;
      mo_label        : Non_empty_string.t;
      mo_occ          : Positive_float.t array;
      mo_coef         : MO_coef.t array;
    } with sexp
  ;;
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { mo_tot_num      : MO_number.t ;
      mo_label        : Non_empty_string.t;
      mo_occ          : Positive_float.t array;
      mo_coef         : MO_coef.t array;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "mo_basis";;

  let read_mo_label () = 
    if not (Ezfio.has_mo_basis_mo_label ()) then
       Ezfio.set_mo_basis_mo_label "Unknown"
    ;
    Ezfio.get_mo_basis_mo_label ()
    |> Non_empty_string.of_string
  ;;

  let read_mo_tot_num () =
    Ezfio.get_mo_basis_mo_tot_num ()
    |> MO_number.of_int
  ;;

  let read_mo_occ () =
    if not (Ezfio.has_mo_basis_mo_label ()) then
      begin
        let elec_alpha_num = Ezfio.get_electrons_elec_alpha_num () 
        and elec_beta_num = Ezfio.get_electrons_elec_beta_num ()
        and mo_tot_num = MO_number.to_int (read_mo_tot_num ()) in
        let data = Array.init mo_tot_num ~f:(fun i ->
          if (i<elec_beta_num) then 2.
          else if (i < elec_alpha_num) then 1.
          else 0.) |> Array.to_list in
        Ezfio.ezfio_array_of_list ~rank:1 
          ~dim:[| mo_tot_num |] ~data:data
        |> Ezfio.set_mo_basis_mo_occ
      end;
    (Ezfio.get_mo_basis_mo_occ () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:Positive_float.of_float
  ;;

  let read_mo_coef () =
    (Ezfio.get_mo_basis_mo_coef () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:MO_coef.of_float
  ;;

  let read () =
    { mo_tot_num      = read_mo_tot_num ();
      mo_label        = read_mo_label () ;
      mo_occ          = read_mo_occ ();
      mo_coef         = read_mo_coef ();
    }
  ;;

  let to_string b =
    Printf.sprintf "
mo_label        = %s
mo_tot_num      = \"%s\"
mo_occ          = %s
mo_coef         = %s
"
    (Non_empty_string.to_string b.mo_label)
    (MO_number.to_string b.mo_tot_num)
    (b.mo_occ |> Array.to_list |> List.map
      ~f:(Positive_float.to_string) |> String.concat ~sep:", " )
    (b.mo_coef |> Array.to_list |> List.map
      ~f:(MO_coef.to_string) |> String.concat ~sep:", " )

end


