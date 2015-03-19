BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "integer" )
T.set_name      ( "N_states" )
T.set_doc       ( "Number of states to consider" )
T.set_ezfio_dir ( "determinants" )
T.set_ezfio_name( "N_states" )
T.set_output    ( "output_dets" )
print T


T.set_name      ( "N_det_max_jacobi" )
T.set_doc       ( "Maximum number of determinants diagonalized by Jacobi" )
T.set_ezfio_name( "N_det_max_jacobi" )
print T

T.set_type      ( "logical" )
T.set_name      ( "read_wf" )
T.set_doc       ( "If true, read the wave function from the EZFIO file" )
T.set_ezfio_name( "read_wf" )
T.set_output    ( "output_dets" )
print T

T.set_type      ( "logical" )
T.set_name      ( "only_single_double_dm" )
T.set_doc       ( "If true, The One body DM is calculated with ignoring the Double<->Doubles extra diag elements" )
T.set_ezfio_name( "only_single_double_dm" )
T.set_output    ( "output_dets" )
print T


T.set_name      ( "s2_eig" )
T.set_doc       ( "Force the wave function to be an eigenfunction of S^2" )
T.set_ezfio_name( "s2_eig" )
print T

END_SHELL

BEGIN_PROVIDER [ integer, N_states_diag ]
  implicit none
  BEGIN_DOC  
!  Number of states to consider for the diagonalization 
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_n_states_diag(has)
  if (has) then
    call ezfio_get_determinants_n_states_diag(N_states_diag)
  else
    N_states_diag = N_states
  endif
  
  call write_time(output_dets)
  call write_int(output_dets, N_states_diag, &
      'N_states_diag')
      

END_PROVIDER

