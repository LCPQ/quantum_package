program s2_eig_restart
 implicit none
 read_wf = .True.
 call routine_2
end

subroutine routine_2
 implicit none
 integer :: i,j,k,l
 use bitmasks
 integer :: n_det_restart,degree
 integer(bit_kind),allocatable :: psi_det_tmp(:,:,:)
 double precision ,allocatable :: psi_coef_tmp(:,:),accu(:)
 integer, allocatable :: index_restart(:)
 allocate(index_restart(N_det))
 print*, 'How many Slater determinants would ou like ?'
 read(5,*)N_det_restart
 do i = 1, N_det_restart
  index_restart(i) = i
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


subroutine routine
 implicit none
  call make_s2_eigenfunction
  TOUCH psi_det psi_coef psi_det_sorted psi_coef_sorted psi_average_norm_contrib_sorted N_det
  call save_wavefunction
end
