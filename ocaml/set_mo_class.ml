open Qputils;;
open Qptypes;;
open Core.Std;;

(*
 * Command-line arguments
 * ----------------------
 *)

let build_mask from upto n_int =
  let from  = MO_number.to_int from
  and upto  = MO_number.to_int upto
  and n_int = N_int_number.to_int n_int
  in
  let rec build_mask bit = function
    | 0 -> []
    | i -> 
        if ( i = upto ) then
          Bit.One::(build_mask Bit.One (i-1))
        else if ( i = from ) then
          Bit.One::(build_mask Bit.Zero (i-1))
        else
          bit::(build_mask bit (i-1))
  in
  let starting_bit = 
    if ( (upto >= n_int*64) || (upto < 0) ) then Bit.One
    else Bit.Zero
  in
  build_mask starting_bit (n_int*64)
  |> List.rev
;;

let mo_tot_num = ref 0;;
let n_int      = ref (N_int_number.of_int 1);;

let apply_mask mask = 
  let full_mask = build_mask (MO_number.of_int 1) (MO_number.of_int !mo_tot_num) !n_int
  in
  let newmask = Bitlist.and_operator full_mask mask
  in
  (* TODO *)
  newmask |> Bitlist.to_string |> print_endline;
  string_of_int !mo_tot_num |> print_endline;
;;



let failure s = 
  raise (Failure s)
;;

let run ?active ?(inactive="[]") ezfio_filename =

  Ezfio.set_file ezfio_filename ;
  if not (Ezfio.has_mo_basis_mo_tot_num ()) then
    failure "mo_basis/mo_tot_num not found" ;

  mo_tot_num := Ezfio.get_mo_basis_mo_tot_num () ;
  n_int := N_int_number.of_int (Ezfio.get_determinants_n_int ()) ;

  let inactive_mask = Range.of_string inactive
    |> List.map ~f:MO_number.of_int
    |> Bitlist.of_mo_number_list !n_int
  and active_mask = 
    let s = 
    match active with
    | Some range -> Range.of_string range
    | None       -> Range.of_string ("[1-"^(Int.to_string !mo_tot_num)^"]")
    in
      List.map ~f:MO_number.of_int s
      |> Bitlist.of_mo_number_list !n_int
  in
  let mask = 
    Bitlist.not_operator inactive_mask
    |> Bitlist.and_operator active_mask
  in apply_mask mask
;;

let ezfio_file =
  let failure filename = 
        eprintf "'%s' is not an EZFIO file.\n%!" filename;
        exit 1
  in
  Command.Spec.Arg_type.create
  (fun filename ->
    match Sys.is_directory filename with
    | `Yes -> 
        begin
          match Sys.is_file (filename / ".version") with
          | `Yes -> filename
          | _ -> failure filename
        end
    | _ -> failure filename
  )
;;

let default range =
  let failure filename = 
        eprintf "'%s' is not a regular file.\n%!" filename;
        exit 1
  in
  Command.Spec.Arg_type.create
  (fun filename ->
    match Sys.is_directory filename with
    | `Yes -> 
        begin
          match Sys.is_file (filename / ".version") with
          | `Yes -> filename
          | _ -> failure filename
        end
    | _ -> failure filename
  )
;;

let spec =
  let open Command.Spec in
  empty 
  +> flag "inactive"  (optional string) ~doc:"range Range of inactive orbitals"
  +> flag "active"  (optional string)   ~doc:"range Range of active orbitals"
  +> anon ("ezfio_filename" %: ezfio_file)
;;

let command = 
    Command.basic 
    ~summary: "Set the active/inactive orbitals in an EZFIO directory"
    ~readme:(fun () ->
      "The range of MOs has the form : \"[36-53,72-107,126-131]\"
        ")
    spec
    (fun inactive active ezfio_filename () -> run ?inactive
    ?active ezfio_filename )
;;

let () =
    Command.run command


(*
let test_module () = 
  let { Ezfio.rank ; Ezfio.dim ; Ezfio.data } = Ezfio.get_bitmasks_generators () in
  let test = 
    Ezfio.flattened_ezfio_data data 
    |> Array.to_list 
    |> List.map Int64.of_int 
    |> Bitlist.of_int64_list 
  in
  print_string (Bitlist.to_string test);
  print_newline ();
  print_string (string_of_int (String.length (Bitlist.to_string test)));
  print_newline ();

  let a = Bitlist.of_int64_list ([-1L;0L]) 
  and b = Bitlist.of_int64_list ([128L;127L])
  in begin
    print_newline ();
    print_newline ();
    Bitlist.to_string a |> print_string;
    print_newline ();
    Bitlist.to_string b |> print_string;
    print_newline ();
    Bitlist.and_operator a b |> Bitlist.to_string |> print_string;
    print_newline ();
    Bitlist.or_operator a b  |> Bitlist.to_string |> print_string;
    print_newline ();
    Bitlist.xor_operator a b |> Bitlist.to_string |> print_string;
  end
;;
*)

(*test_module ();;*)
