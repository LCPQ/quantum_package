program cis
  implicit none
  read_wf = .False.
  SOFT_TOUCH read_wf
  call run
end

subroutine run
  implicit none
  integer :: i

  call H_apply_cis
  print *,  'N_det = ', N_det
  do i = 1,N_states
   print *,  'energy  = ',CI_energy(i) 
   print *,  'E_corr  = ',CI_electronic_energy(i) - ref_bitmask_energy
  enddo
  psi_coef = ci_eigenvectors
  SOFT_TOUCH psi_coef
  call save_wavefunction

end
