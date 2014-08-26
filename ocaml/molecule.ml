open Core.Std ;;
open Qptypes ;;

exception MultiplicityError of string;;

type t = {
  nuclei     : Atom.t list ;
  elec_alpha : Positive_int.t ;
  elec_beta  : Positive_int.t ;
}

let get_charge { nuclei  ; elec_alpha ; elec_beta } =
  let result = Positive_int.(to_int elec_alpha + to_int elec_beta) in
  let rec nucl_charge = function
  | a::rest -> Atom.(Charge.to_float a.charge) +. nucl_charge rest
  | [] -> 0.
  in
  nucl_charge nuclei  -. (Float.of_int result)
;;

let get_multiplicity m = 
  Multiplicity.of_alpha_beta m.elec_alpha m.elec_beta
;;

let name m = 
  let cm = Float.to_int (get_charge m) in
  let c = 
     match cm with
     | 0 -> ""
     | 1 -> " (+)"
     | (-1) -> " (-)"
     | i when i>1 -> Printf.sprintf " (%d+)" i
     | i -> Printf.sprintf " (%d-)" (-i)
  in
  let mult = Multiplicity.to_string (get_multiplicity m) in
  let { nuclei  ; elec_alpha ; elec_beta } = m in
  let rec build_list accu = function
  | a::rest ->
      begin
        let e = a.Atom.element in
        match (List.Assoc.find accu e) with
        | None   -> build_list (List.Assoc.add accu e 1) rest
        | Some i -> build_list (List.Assoc.add accu e (i+1)) rest
      end
  | [] -> accu
  in
  let rec build_name accu = function
  | (a, n)::rest ->
    let a = Element.to_string a in
    begin
      match n with 
      | 1 -> build_name (a::accu) rest
      | i when i>1 -> 
        let tmp = Printf.sprintf "%s%d" a i
        in build_name (tmp::accu) rest
      | _ -> assert false
    end
  | [] -> accu
  in
  let result = build_list [] nuclei |> build_name [c ; ", " ; mult]
  in
  String.concat (result)
;;

let to_string m =
  let { nuclei  ; elec_alpha ; elec_beta } = m in
  let n = List.length nuclei in
  let title = name m in
  [ Int.to_string n ; title ] @ (List.map ~f:Atom.to_string nuclei)
  |> String.concat ~sep:"\n"
;;

let of_xyz_string
    ?(charge=0) ?(multiplicity=(Multiplicity.of_int 1))
    s =
  let l = String.split s ~on:'\n'
       |> List.filter ~f:(fun x -> x <> "")
       |> List.map ~f:Atom.of_string 
  in
  let ne = ( get_charge { 
        nuclei=l ;
        elec_alpha=(Positive_int.of_int 0) ;
        elec_beta=(Positive_int.of_int 0) } 
      |> Float.to_int 
      )- charge 
      |> Positive_int.of_int 
  in
  let (na,nb) = Multiplicity.to_alpha_beta ne multiplicity in
  let result = 
  { nuclei = l ;
    elec_alpha = (Positive_int.of_int na) ;
    elec_beta  = (Positive_int.of_int nb) }
  in
  if ((get_multiplicity result) <> multiplicity) then
     let msg = Printf.sprintf
      "With %d electrons multiplicity %d is impossible"
      (Positive_int.to_int ne)
      (Multiplicity.to_int multiplicity)
     in
     raise (MultiplicityError msg);
  else () ;
  result
;;


let of_xyz_file
    ?(charge=0) ?(multiplicity=(Multiplicity.of_int 1))
    filename =
  let (_,buffer) = In_channel.read_all filename 
  |> String.lsplit2_exn ~on:'\n' in
  let (_,buffer) = String.lsplit2_exn buffer ~on:'\n' in
  of_xyz_string ~charge:charge ~multiplicity:multiplicity buffer
;;
