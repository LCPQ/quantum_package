program cisd
  implicit none
  integer :: i

  print *,   'HF      = ', HF_energy
  print *,  'N_states = ', N_states
  call H_apply_cisd
  print *,  'N_det = ', N_det
  do i = 1,N_states
   print *,  'energy  = ',CI_energy(i) 
   print *,  'E_corr  = ',CI_electronic_energy(i) - ref_bitmask_energy
  enddo

! call CISD_SC2(psi_det,psi_coef,eigvalues,size(psi_coef,1),N_det,N_states,N_int)
! do i = 1, N_states
!  print*,'eigvalues(i) = ',eigvalues(i)
! enddo
end
