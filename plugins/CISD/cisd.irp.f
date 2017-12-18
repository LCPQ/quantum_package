program cisd
  implicit none
  integer :: i

  print *,   'HF      = ', HF_energy
  print *,  'N_states = ', N_states
  N_det = 1
  touch psi_det psi_coef N_det
  call H_apply_cisd
  print *,  'N_det = ', N_det
  do i = 1,N_states
   print *,  'energy  = ',CI_energy(i) 
   print *,  'E_corr  = ',CI_electronic_energy(i) - ref_bitmask_energy
  enddo

  call ezfio_set_cisd_energy(CI_energy(1))
  psi_coef = ci_eigenvectors
  SOFT_TOUCH psi_coef
  call save_wavefunction
end
