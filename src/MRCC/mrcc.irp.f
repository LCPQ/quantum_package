program mrcc
  implicit none
  read_wf = .True.
  TOUCH read_wf
  call run
end

subroutine run
  implicit none

  print *, N_det
  print *, N_det_cas
  print *, N_det_sd

!  call update_generators
  integer :: i
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  psi_cas_coefs(i,:)
    call debug_det(psi_cas(1,1,i),N_int)
  enddo

!  print *,  'SD'
!  print *,  '=='
!  do i=1,N_det_sd
!    print *,  psi_sd_coefs(i,:)
!    call debug_det(psi_sd(1,1,i),N_int)
!  enddo
!
  print *,  'MRCC'
  print *,  '===='
  print *,  ci_energy(:)
  print *,  h_matrix_all_dets(3,3), delta_ij(3,3,1)
  print *,  h_matrix_all_dets(3,3), delta_ij(3,3,1)
  print *,  ci_energy_dressed(:)
!  print *,  'max', maxval(delta_ij_sd)
!  print *,  'min', minval(delta_ij_sd)
!
!  do i=1,N_det
!     print '(10(F10.6,X))',  delta_ij(i,1:N_det,1)
!  enddo
end
