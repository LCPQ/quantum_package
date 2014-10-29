open Qptypes;;
open Qputils;;
open Core.Std;;

module Nuclei : sig
  type t = 
    { nucl_num        : Nucl_number.t ;
      nucl_label      : Element.t array;
      nucl_charge     : Charge.t array;
      nucl_coord      : Point3d.t array;
    } with sexp
  ;;
  val read : unit -> t
  val debug : t -> string
  val to_string : t -> string
end = struct
  type t = 
    { nucl_num        : Nucl_number.t ;
      nucl_label      : Element.t array;
      nucl_charge     : Charge.t array;
      nucl_coord      : Point3d.t array;
    } with sexp
  ;;

  let get_default = Qpackage.get_ezfio_default "nuclei";;

  let read_nucl_num () = 
    Ezfio.get_nuclei_nucl_num ()
    |> Nucl_number.of_int
  ;;

  let read_nucl_label () =
    (Ezfio.get_nuclei_nucl_label ()).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:Element.of_string 
  ;;

  let read_nucl_charge () =
    (Ezfio.get_nuclei_nucl_charge () ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    |> Array.map ~f:Charge.of_float
  ;;

  let read_nucl_coord () =
    let nucl_num = Nucl_number.to_int (read_nucl_num ()) in
    let raw_data = 
    (Ezfio.get_nuclei_nucl_coord() ).Ezfio.data
    |> Ezfio.flattened_ezfio_data
    in
    let zero = Point3d.of_string Units.Bohr "0. 0. 0." in
    let result = Array.create nucl_num zero in
    for i=0 to (nucl_num-1)
    do
      result.(i) <- Point3d.({ x=raw_data.(i);
                              y=raw_data.(nucl_num+i);
                              z=raw_data.(2*nucl_num+i); });
    done;
    result
  ;;

  let read () =
    { nucl_num        = read_nucl_num ();
      nucl_label      = read_nucl_label () ;
      nucl_charge     = read_nucl_charge ();
      nucl_coord      = read_nucl_coord ();
    }
  ;;

  let debug b =
    Printf.sprintf "
nucl_num         = %s
nucl_label       = %s
nucl_charge      = %s
nucl_coord       = %s
"
    (Nucl_number.to_string b.nucl_num)
    (b.nucl_label |> Array.to_list |> List.map
      ~f:(Element.to_string) |> String.concat ~sep:", " )
    (b.nucl_charge |> Array.to_list |> List.map
      ~f:(Charge.to_string) |> String.concat ~sep:", " )
    (b.nucl_coord  |> Array.to_list |> List.map
      ~f:(Point3d.to_string Units.Bohr) |> String.concat ~sep:"\n" )
  ;;

   let to_string b = 
     let nucl_num = Nucl_number.to_int b.nucl_num in
     let text = 
       ( Printf.sprintf "  %d\n  "
         nucl_num 
       ) :: (
       List.init nucl_num ~f:(fun i->
         Printf.sprintf "  %-3s  %d   %s"
          (b.nucl_label.(i)  |> Element.to_string)
          (b.nucl_charge.(i) |> Charge.to_int )
          (b.nucl_coord.(i)  |> Point3d.to_string Units.Angstrom) )
      ) |> String.concat ~sep:"\n"
     in
     Printf.sprintf "
Nuclear coordinates in xyz format (Angstroms) ::

%s

" text
     
;;
end


