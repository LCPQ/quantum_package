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
  
  call write_time(output_determinants)
  call write_int(output_determinants, N_states_diag, &
      'N_states_diag')
      

END_PROVIDER

