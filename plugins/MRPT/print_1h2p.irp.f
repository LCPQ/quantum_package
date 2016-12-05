program print_1h2p
 implicit none
 read_wf = .True.
 touch read_wf
 call routine_2
end

subroutine routine_2
 implicit none
 integer :: i,j,degree
 double precision :: hij
!provide   one_creat_virt
 do i =1, n_act_orb
  write(*,'(I3,x,100(F16.10,X))')i,one_creat(i,:,1)
! write(*,'(I3,x,100(F16.10,X))')i,one_anhil_one_creat(1,4,1,2,1)
! 
 enddo


end

subroutine routine
 implicit none
 double precision,allocatable :: matrix_1h2p(:,:,:) 
 double precision :: accu(2)
 allocate (matrix_1h2p(N_det_ref,N_det_ref,N_states))
 integer :: i,j,istate
 accu = 0.d0
 matrix_1h2p = 0.d0
 call H_apply_mrpt_2p(matrix_1h2p,N_det_ref)
 do istate = 1, N_states
  do i = 1, N_det
   do j = 1, N_det 
    accu(istate) += matrix_1h2p(i,j,istate) * psi_coef(i,istate) * psi_coef(j,istate)
   enddo
  enddo
  print*,accu(istate)
 enddo

 deallocate (matrix_1h2p)
end
