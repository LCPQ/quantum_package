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
 double precision, allocatable :: H_naked(:,:)
 double precision, allocatable :: H_dressed(:,:)
 double precision, allocatable :: H_print(:,:)
 double precision :: accu_norm
 allocate (H_dressed(max_number_ionic+1,max_number_ionic+1))
 allocate (H_print(min_number_ionic:max_number_ionic,min_number_ionic:max_number_ionic))
 allocate (H_naked(max_number_ionic+1,max_number_ionic+1))
 allocate (psi_restart_ref_normalized(min_number_ionic:max_number_ionic))
 allocate (psi_ref_zeroth_order(min_number_ionic:max_number_ionic))
 print*,'# nuclear_repulsion = ',nuclear_repulsion 
 allocate (psi_ref_dressed(min_number_ionic:max_number_ionic))
 allocate (eigvalues(max_number_ionic+1))
 allocate (eigvectors(max_number_ionic+1,max_number_ionic+1))
 
 do istate= 1, N_states 
   print*,'ISTATE = ',istate
   do i = min_number_ionic,max_number_ionic
    do j = min_number_ionic,max_number_ionic
     H_print(i,j) = H_OVB_naked(j,i,istate)
    enddo
   enddo
   do i = min_number_ionic,max_number_ionic
    H_print(i,i) -= H_OVB_naked(min_number_ionic,min_number_ionic,istate)
   enddo
  
   print*,'Ref Hamiltonian matrix emelent = ',H_OVB_naked(min_number_ionic,min_number_ionic,istate)
   print*,'-------------------'
   print*,'-------------------'
   print*,'CAS MATRIX         '
   print*,''
   do i = min_number_ionic,max_number_ionic
    write(*,'(I4,X,10(F8.5 ,4X))')i, H_print(i,:)
   enddo
  print*,'CAS MATRIX DRESSING'
  print*,''
  do i = min_number_ionic,max_number_ionic
   write(*,'(I4,X,10(F8.5 ,4X))')i, H_OVB_dressing(i,:,istate)
  enddo
  print*,''
  print*,'-------------------'
  print*,'-------------------'
  print*,'CAS MATRIX DRESSED '
  print*,''
  do i = min_number_ionic,max_number_ionic
   do j = min_number_ionic,max_number_ionic
    H_print(i,j) = H_OVB_total_dressed(j,i,istate)
   enddo
  enddo
  do i = min_number_ionic,max_number_ionic
   H_print(i,i) -= H_OVB_total_dressed(min_number_ionic,min_number_ionic,istate)
  enddo
  do i = min_number_ionic,max_number_ionic
   write(*,'(I4,X,10(F8.5 ,4X))')i, H_print(i,:)
  enddo
  print*,''
  do i = min_number_ionic,max_number_ionic
   do j = min_number_ionic,max_number_ionic
    H_dressed(j+1,i+1) = H_OVB_total_dressed(i,j,istate)
    H_naked(j+1,i+1) = H_OVB_naked(i,j,istate)
   enddo
  enddo
 
  call lapack_diagd(eigvalues,eigvectors,H_naked,max_number_ionic+1,max_number_ionic+1)
  print*,'E+PT2 = ',eigvalues(istate) + nuclear_repulsion
  do i = min_number_ionic,max_number_ionic
   psi_ref_zeroth_order(i) = eigvectors(i+1,istate)
  enddo
  
 
  
  call lapack_diagd(eigvalues,eigvectors,H_dressed,max_number_ionic+1,max_number_ionic+1)
  do i = min_number_ionic,max_number_ionic
   psi_ref_dressed(i) = eigvectors(i+1,istate)
  enddo
  print*,'E+PT2 = ',eigvalues(istate) + nuclear_repulsion
  do i = min_number_ionic,max_number_ionic
   write(*,'(10(F10.7 ,4X))') psi_ref_dressed(i)/psi_ref_dressed(min_number_ionic) ,psi_ref_zeroth_order(i)/psi_ref_zeroth_order(min_number_ionic)
  enddo
 enddo
 
 deallocate (H_dressed)
 deallocate (H_naked)
 deallocate (psi_restart_ref_normalized)
 deallocate (psi_ref_zeroth_order)
 deallocate (psi_ref_dressed)

 deallocate (eigvalues)
 deallocate (eigvectors)

end
