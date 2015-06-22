program cisd
  implicit none
  integer :: i

  diag_algorithm = "Lapack"
  touch diag_algorithm
  print *,   'HF      = ', HF_energy
  print *,  'N_states = ', N_states
  call H_apply_cisd
  print *,  'N_det = ', N_det
  do i = 1,N_states
   print *,  'energy  = ',CI_energy(i) 
   print *,  'E_corr  = ',CI_electronic_energy(i) - ref_bitmask_energy
  enddo

end
