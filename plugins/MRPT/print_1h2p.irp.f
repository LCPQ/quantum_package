program print_1h2p
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 provide one_anhil_one_creat_inact_virt

end

subroutine routine_2
 implicit none
 integer :: i,j,degree
 double precision :: hij
 do i =1, n_core_inact_orb 
  write(*,'(I3,x,100(F16.10,X))')list_core_inact(i),fock_core_inactive_total_spin_trace(list_core_inact(i),1)
 enddo
 print*,''
 do i =1, n_virt_orb
  write(*,'(I3,x,100(F16.10,X))')list_virt(i),fock_virt_total_spin_trace(list_virt(i),1)
 enddo
 stop
 do i = 1, n_virt_orb
  do j = 1, n_inact_orb 
  if(dabs(one_anhil_one_creat_inact_virt(j,i,1)) .lt. 1.d-10)cycle
  write(*,'(I3,x,I3,X,100(F16.10,X))')list_virt(i),list_inact(j),one_anhil_one_creat_inact_virt(j,i,1)
  enddo
 enddo


end

subroutine routine_3
 implicit none
 double precision,allocatable :: matrix_1h2p(:,:,:) 
 double precision :: accu(2)
 allocate (matrix_1h2p(N_det_ref,N_det_ref,N_states))
 integer :: i,j,istate
 accu = 0.d0
 matrix_1h2p = 0.d0
!call H_apply_mrpt_1h2p(matrix_1h2p,N_det_ref)
 call give_1h2p_contrib(matrix_1h2p)
 do istate = 1, N_states
  do i = 1, N_det
   do j = 1, N_det 
    accu(istate) += matrix_1h2p(i,j,istate) * psi_coef(i,istate) * psi_coef(j,istate)
   enddo
  enddo
  print*,accu(istate)
 enddo
 call contrib_1h2p_dm_based(accu)
 print*,accu(:)

 deallocate (matrix_1h2p)
end
