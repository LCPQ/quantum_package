program print
 read_wf = .True.
 touch read_wf
 call provide_all_stuffs
end
subroutine provide_all_stuffs
 implicit none
 provide ref_hamiltonian_matrix dressing_ref_hamiltonian
 integer :: i,j,istate
 double precision, allocatable :: psi_restart_ref_normalized(:),psi_ref_zeroth_order(:),psi_ref_dressed(:)
 double precision, allocatable :: eigvalues(:),eigvectors(:,:)
 double precision, allocatable :: H_dressed(:,:)
 double precision, allocatable :: H_print(:,:)
 double precision :: accu_norm
 allocate (H_dressed(N_det_ref,N_det_ref))
 allocate (H_print(N_det_ref,N_det_ref))
 allocate (psi_restart_ref_normalized(N_det_ref))
 allocate (psi_ref_zeroth_order(N_det_ref))
 print*,'# nuclear_repulsion = ',nuclear_repulsion 
 allocate (psi_ref_dressed(N_det_ref))
 allocate (eigvalues(N_det_ref))
 allocate (eigvectors(N_det_ref,N_det_ref))


 
 do istate= 1, N_states 
  do i = 1, N_det_ref 
   do j = 1, N_det_ref
    H_print(i,j) = ref_hamiltonian_matrix(j,i)
   enddo
  enddo
  do i = 1, N_det_ref
   H_print(i,i) -= ref_hamiltonian_matrix(1,1)
  enddo
  print*,'Ref Hamiltonian matrix emelent = ',ref_hamiltonian_matrix(1,1)
  print*,'ISTATE = ',istate
  accu_norm = 0.d0
  do i = 1, N_det_ref 
   accu_norm += psi_ref_coef(i,1) * psi_ref_coef(i,1)
  enddo
  print*,'accu_norm = ',accu_norm
  accu_norm = 1.d0/dsqrt(accu_norm)
  do i = 1, N_det_ref 
   psi_restart_ref_normalized(i) = psi_ref_coef(i,istate)* accu_norm
  enddo
  print*,'-------------------'
  print*,'-------------------'
  print*,'CAS MATRIX         '
  print*,''
  do i = 1, N_det_ref
   write(*,'(10(F8.5 ,4X))') H_print(i,:)
  enddo
  print*,''
  print*,'-------------------'
  print*,'-------------------'
  print*,'CAS MATRIX DRESSING'
  print*,''
  do i = 1, N_det_ref
   write(*,'(10(F8.5 ,4X))') dressing_ref_hamiltonian(i,:,istate)
  enddo
  print*,''
  print*,'-------------------'
  print*,'-------------------'
  do i = 1, N_det_ref
   do j = 1, N_det_ref
    H_dressed(j,i) = ref_hamiltonian_matrix(j,i) + dressing_ref_hamiltonian(j,i,istate)
    H_print(i,j) += dressing_ref_hamiltonian(j,i,istate)
   enddo
  enddo
  print*,''
  print*,'-------------------'
  print*,'-------------------'
  print*,'TOTAL DRESSED H MATRIX '
  print*,''
  do i = 1, N_det_ref
   write(*,'(10(F8.5 ,4X))') H_print(i,:)
  enddo
  print*,''
  print*,''
  print*,''
 
 
  call lapack_diagd(eigvalues,eigvectors,ref_hamiltonian_matrix,n_det_ref,n_det_ref)
  do i = 1, N_det_ref
   psi_ref_zeroth_order(i) = eigvectors(i,istate)
  enddo
 
  
  call lapack_diagd(eigvalues,eigvectors,H_dressed,n_det_ref,n_det_ref)
  do i = 1, N_det_ref
   psi_ref_dressed(i) = eigvectors(i,istate)
  enddo
  print*,'E+PT2 = ',eigvalues(istate) + nuclear_repulsion
  do i = 1, N_det_ref
   write(*,'(10(F10.7 ,4X))') psi_ref_coef(i,istate)/psi_ref_coef(1,istate), psi_ref_dressed(i)/psi_ref_dressed(1),psi_ref_zeroth_order(i)/psi_ref_zeroth_order(1)
  enddo
 enddo
 
 deallocate (H_dressed)
 deallocate (H_print)
 deallocate (psi_restart_ref_normalized)
 deallocate (psi_ref_zeroth_order)
 deallocate (psi_ref_dressed)

 deallocate (eigvalues)
 deallocate (eigvectors)

end
