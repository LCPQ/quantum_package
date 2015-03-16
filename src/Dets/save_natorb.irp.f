program save_natorb
  read_wf = .True.
  touch read_wf
  call outine
end

subroutine outine 
implicit none
integer :: i,j
 print*,'In the routine'
 print*,'N_det = ',N_det
!do i = 1, N_det
! print*,'i = ',i
! print*,'psi_coef(i,1) = ',psi_coef(i,1)
! call debug_det(psi_det(1,1,i),N_int)
!enddo
  call save_natural_mos
end

