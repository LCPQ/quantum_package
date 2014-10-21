open Qptypes;;
open Qputils;;
open Core.Std;;

module Ao_basis : sig
  type t = 
    { ao_basis        : string ;
      ao_num          : AO_number.t ;
      ao_prim_num     : AO_prim_number.t array;
      ao_prim_num_max : AO_prim_number.t;
      ao_nucl         : Nucl_number.t array;
      ao_power        : Symmetry.Xyz.t array;
      ao_coef         : AO_coef.t array;
      ao_expo         : AO_expo.t array;
    }
  ;;
  val read : unit -> t
  val to_string : t -> string
end = struct
  type t = 
    { ao_basis        : string ;
      ao_num          : AO_number.t ;
      ao_prim_num     : AO_prim_number.t array;
      ao_prim_num_max : AO_prim_number.t;
      ao_nucl         : Nucl_number.t array;
      ao_power        : Symmetry.Xyz.t array;
      ao_coef         : AO_coef.t array;
      ao_expo         : AO_expo.t array;
    }
  ;;

  let get_default = Qpackage.get_ezfio_default "ao_basis";;

  let read_ao_basis () = 
    if not (Ezfio.has_ao_basis_ao_basis ()) then
       Ezfio.set_ao_basis_ao_basis ""
    ;
    Ezfio.get_ao_basis_ao_basis ()
  ;;

  let read_ao_num () =
    Ezfio.get_ao_basis_ao_num ()
    |> AO_number.of_int
  ;;

  let read_ao_prim_num () =
    (Ezfio.get_ao_basis_ao_prim_num () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:AO_prim_number.of_int
  ;;

  let read_ao_prim_num_max () =
    (Ezfio.get_ao_basis_ao_prim_num () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.fold ~f:(fun x y -> if x>y then x else y) ~init:0
    |> AO_prim_number.of_int
  ;;

  let read_ao_nucl () =
    (Ezfio.get_ao_basis_ao_nucl () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:Nucl_number.of_int
  ;;

  let read_ao_power () =
    let x = Ezfio.get_ao_basis_ao_power () in
    let dim = x.Ezfio.dim.(0) in
    let data = Ezfio.flattened_ezfio_data x.Ezfio.data in
    let result = Array.init dim ~f:(fun x -> "") in
    for i=1 to dim
    do
      if (data.(i-1) > 0) then
        result.(i-1) <- result.(i-1)^"x"^(Int.to_string data.(i-1));
      if (data.(dim+i-1) > 0) then
        result.(i-1) <- result.(i-1)^"y"^(Int.to_string data.(dim+i-1));
      if (data.(2*dim+i-1) > 0) then
        result.(i-1) <- result.(i-1)^"z"^(Int.to_string data.(2*dim+i-1));
    done;
    Array.map ~f:Symmetry.Xyz.of_string result
  ;;

  let read_ao_coef () =
    (Ezfio.get_ao_basis_ao_coef () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:AO_coef.of_float
  ;;

  let read_ao_expo () =
    (Ezfio.get_ao_basis_ao_expo () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:AO_expo.of_float
  ;;

  let read () =
    { ao_basis        = read_ao_basis ();
      ao_num          = read_ao_num () ;
      ao_prim_num     = read_ao_prim_num ();
      ao_prim_num_max = read_ao_prim_num_max ();
      ao_nucl         = read_ao_nucl ();
      ao_power        = read_ao_power ();
      ao_coef         = read_ao_coef () ;
      ao_expo         = read_ao_expo () ;
    }
  ;;

  let to_string b =
    Printf.sprintf "
ao_basis        = \"%s\"
ao_num          = %s
ao_prim_num     = %s
ao_prim_num_max = %s
ao_nucl         = %s
ao_power        = %s
ao_coef         = %s
ao_expo         = %s
"
    b.ao_basis
    (AO_number.to_string b.ao_num)
    (b.ao_prim_num |> Array.to_list |> List.map
      ~f:(AO_prim_number.to_string) |> String.concat ~sep:", " )
    (AO_prim_number.to_string b.ao_prim_num_max)
    (b.ao_nucl |> Array.to_list |> List.map ~f:Nucl_number.to_string |>
      String.concat ~sep:", ")
    (b.ao_power |> Array.to_list |> List.map ~f:(fun x->
      "("^(Symmetry.Xyz.to_string x)^")" )|> String.concat ~sep:", ")
    (b.ao_coef  |> Array.to_list |> List.map ~f:AO_coef.to_string
      |> String.concat ~sep:", ")
    (b.ao_expo  |> Array.to_list |> List.map ~f:AO_expo.to_string
      |> String.concat ~sep:", ")

end


