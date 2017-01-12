program save_only_singles
 implicit none
 read_wf = .True. 
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 integer :: i,j,k,l
 use bitmasks
 integer :: n_det_restart,degree
 integer(bit_kind),allocatable :: psi_det_tmp(:,:,:)
 double precision ,allocatable :: psi_coef_tmp(:,:),accu(:)
 integer, allocatable :: index_restart(:)
 allocate(index_restart(N_det))
 N_det_restart = 0
 do i = 1, N_det
  call get_excitation_degree(psi_det(1,1,1),psi_det(1,1,i),degree,N_int)
  if(degree == 0 .or. degree==1)then
   N_det_restart +=1
   index_restart(N_det_restart) = i
   cycle
  endif
 enddo
 allocate (psi_det_tmp(N_int,2,N_det_restart),psi_coef_tmp(N_det_restart,N_states),accu(N_states))
 accu = 0.d0
 do i = 1, N_det_restart
  do j = 1, N_int
   psi_det_tmp(j,1,i) = psi_det(j,1,index_restart(i))
   psi_det_tmp(j,2,i) = psi_det(j,2,index_restart(i))
  enddo
  do j = 1,N_states
   psi_coef_tmp(i,j) = psi_coef(index_restart(i),j)
   accu(j) += psi_coef_tmp(i,j) * psi_coef_tmp(i,j)
  enddo
 enddo
 do j = 1, N_states
  accu(j) = 1.d0/dsqrt(accu(j))
 enddo
 do j = 1,N_states
  do i = 1, N_det_restart
   psi_coef_tmp(i,j) = psi_coef_tmp(i,j) * accu(j)
  enddo
 enddo
 call save_wavefunction_general(N_det_restart,N_states,psi_det_tmp,N_det_restart,psi_coef_tmp)

 deallocate (psi_det_tmp,psi_coef_tmp,accu,index_restart)
 
end 
