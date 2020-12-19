program diag_and_save
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 integer :: igood_state_1,igood_state_2
 double precision, allocatable :: psi_coef_tmp(:,:)
 integer :: i
 print*,'N_det = ',N_det
!call diagonalize_CI
 write(*,*)'Which couple of states would you like to save ?'
 read(5,*)igood_state_1,igood_state_2
 allocate(psi_coef_tmp(n_det,2))
 do i = 1, N_det
  psi_coef_tmp(i,1) = psi_coef(i,igood_state_1)
  psi_coef_tmp(i,2) = psi_coef(i,igood_state_2)
 enddo
 call save_wavefunction_general(N_det,2,psi_det,n_det,psi_coef_tmp)
 deallocate(psi_coef_tmp)



end
