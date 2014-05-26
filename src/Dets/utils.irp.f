BEGIN_PROVIDER [ double precision, H_matrix_all_dets,(N_det,N_det) ]
 implicit none
 BEGIN_DOC
 ! H matrix on the basis of the slater deter;inants defined by psi_det
 END_DOC
 integer :: i,j
 double precision :: hij
 do i =1,N_det
  do j =i,N_det
   call  i_H_j(psi_det(1,1,i),psi_det(1,1,j),N_int,hij)
   H_matrix_all_dets(i,j) = hij
   H_matrix_all_dets(j,i) = hij
  enddo
 enddo
END_PROVIDER

subroutine remove_small_contributions
  implicit none
  BEGIN_DOC
!  Remove determinants with small contributions
  END_DOC
  integer :: i,j,k, N_removed
  logical keep
  N_removed = 0
  do i=N_det,1,-1
    keep = .False.
    do j=1,N_states
      keep = keep .or. (dabs(psi_coef(i,j)) > selection_criterion_min)
    enddo
    if (.not.keep) then
      do k=i+1,N_det
        do j=1,N_int
           psi_det(j,1,k-1) = psi_det(j,1,k)
           psi_det(j,2,k-1) = psi_det(j,2,k)
        enddo
      enddo
      do j=1,N_states
        do k=i+1,N_det
           psi_coef(k-1,j) = psi_coef(k,j)
        enddo
      enddo
      N_removed += 1
    endif
  enddo
  if (N_removed > 0) then
    N_det -= N_removed
    call write_int(output_dets,N_removed, 'Removed determinants')
  endif
end
