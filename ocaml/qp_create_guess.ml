open Qputils
open Qptypes
open Core

let run ~multiplicity ezfio_file =
  if (not (Sys.file_exists_exn ezfio_file)) then
    failwith ("EZFIO directory "^ezfio_file^" not found");
  Ezfio.set_file ezfio_file;
  let d =
    Input.Determinants_by_hand.read ()
  in
  let m =
    Multiplicity.of_int multiplicity
  in
  let ne = 
    Ezfio.get_electrons_elec_alpha_num () +
    Ezfio.get_electrons_elec_beta_num () 
    |> Elec_number.of_int
  in
  let alpha, beta = 
    let (a,b) = 
      Multiplicity.to_alpha_beta ne m 
    in
    (Elec_alpha_number.to_int a, Elec_beta_number.to_int b)
  in
  let n_open_shells =
    alpha - beta
  in
  let mo_tot_num = 
    Ezfio.get_mo_basis_mo_tot_num ()
  in
  let build_list_of_dets ne n_closed n_open =
    let init = 
      Array.create ~len:n_closed Bit.One 
      |> Array.to_list
    in
    let rec set_electron accu = function
    | 1 ->  [ Bit.One :: accu ]
    | i -> 
          assert (i>1);
          let rest =
            set_electron (Bit.Zero :: accu) (i-1)
          in
          (Bit.One::accu) :: rest
    in
    let rec extend accu = function 
    | 0 -> List.rev accu
    | i -> extend (Bit.Zero::accu) (i-1)
    in
    let rec set_n_electrons accu imax = function
    | 0 -> []
    | 1 -> set_electron accu imax
    | i ->
          assert (i>1);
          let l =
            set_electron accu (imax-1)
          in
          List.map ~f:(fun x -> set_n_electrons x (imax-1) (i-1)) l
          |> List.concat
    in
    set_n_electrons init n_open ne
          |> List.filter ~f:(fun x -> List.length x <= n_closed+n_open) 
          |> List.map ~f:(fun x -> extend x (((mo_tot_num-1)/64+1)*64 - List.length x))
  in

  let alpha_new = 
    (Elec_number.to_int ne + 1)/2
  and beta_new = 
    Elec_number.to_int ne/2
  in
  let l_alpha = 
    build_list_of_dets ((alpha-beta+1)/2) beta n_open_shells
  in 
  let l_beta = 
    if alpha_new = beta_new then
      l_alpha
    else
      build_list_of_dets ((alpha-beta)/2)beta n_open_shells 
  in

  let n_int = 
    Bitlist.n_int_of_mo_tot_num mo_tot_num
  in
  let determinants = 
    List.map l_alpha ~f:(fun x -> List.map l_beta ~f:(fun y -> (x,y) ))
    |> List.concat
    |> List.map ~f:(fun pair -> Determinant.of_bitlist_couple ~n_int
      ~alpha:(Elec_alpha_number.of_int alpha_new)
      ~beta:(Elec_beta_number.of_int beta_new) pair )
  in

  let c = 
    Array.init (List.length determinants) (fun _ -> Det_coef.of_float ((Random.float 2.)-.1.))
  in
 
  determinants 
  |> List.map ~f:(fun x -> Determinant.to_string ~mo_tot_num:(MO_number.of_int mo_tot_num) x) 
  |> List.iter ~f:(fun x -> Printf.printf "%s\n\n%!" x);

  let l =
    List.length determinants
  in
  if l > 0 then
    begin
      let d = 
        let s = (Float.of_int (alpha - beta)) *. 0.5 in
        let open Input.Determinants_by_hand in
        { d with n_int ; 
          n_det = Det_number.of_int ~min:1 ~max:l  l;
          expected_s2 = Positive_float.of_float (s *. (s +. 1.)) ;
          psi_coef = c;
          psi_det = Array.of_list determinants;
        }
      in
      Input.Determinants_by_hand.write d;
      Ezfio.set_determinants_read_wf true
    end
  else
    Ezfio.set_determinants_read_wf false
  


let spec =
  let open Command.Spec in
  empty
  +> flag "m" (required int)
     ~doc:"int Spin multiplicity"
  +> anon ("ezfio_file" %: string)

let () = 
  Command.basic
  ~summary: "Quantum Package command"
  ~readme:( fun () -> "
Creates an open-shell multiplet initial guess\n\n" )
  spec
  (fun multiplicity ezfio_file () ->
    run ~multiplicity ezfio_file
  )
  |> Command.run   ~version: Git.sha1   ~build_info: Git.message



