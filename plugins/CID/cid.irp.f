program cid
  implicit none
  integer :: i

  print *,   'HF      = ', HF_energy
  print *,  'N_states = ', N_states
  N_det = 1
  touch psi_det psi_coef N_det
  call H_apply_cid
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
