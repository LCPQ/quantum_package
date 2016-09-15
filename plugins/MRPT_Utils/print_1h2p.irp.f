program print_1h2p
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 double precision,allocatable :: matrix_1h2p(:,:,:) 
 allocate (matrix_1h2p(N_det,N_det,N_states))
 integer :: i,j,istate
 do i = 1, N_det
  do j = 1, N_det
   do istate = 1, N_states
    matrix_1h2p(i,j,istate) = 0.d0
   enddo
  enddo
 enddo
 call give_1h2p_contrib_sec_order(matrix_1h2p)
 double precision :: accu
 accu = 0.d0
 do i = 1, N_det
  do j = 1, N_det 
   accu += matrix_1h2p(i,j,1) * psi_coef(i,1) * psi_coef(j,1)
  enddo
 enddo
 print*, 'accu', accu

 deallocate (matrix_1h2p)
end
