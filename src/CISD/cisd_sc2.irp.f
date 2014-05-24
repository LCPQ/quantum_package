program cisd
  implicit none
  integer :: i, j

  print *,   'HF      = ', HF_energy
  print *,  'N_states = ', N_states
  call H_apply_cisd
  print *,  'N_det = ', N_det
  do i = 1,N_states
   print *,  'energy  = ',CI_energy(i) 
   print *,  'E_corr  = ',CI_electronic_energy(i) - ref_bitmask_energy
   do j=1,N_det
     psi_coef(j,i) = CI_eigenvectors(j,i)
   enddo
 enddo
 SOFT_TOUCH CI_electronic_energy CI_eigenvectors
 call CISD_SC2(psi_det,psi_coef,CI_electronic_energy,size(psi_coef,1),N_det,N_states,N_int)
 TOUCH CI_electronic_energy CI_eigenvectors

 do i = 1, N_states
  print*,'eigvalues(i) = ',CI_energy(i)
 enddo
end
