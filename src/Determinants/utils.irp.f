BEGIN_PROVIDER [ double precision, H_matrix_all_dets,(N_det,N_det) ]
  use bitmasks
 implicit none
 BEGIN_DOC
 ! H matrix on the basis of the slater determinants defined by psi_det
 END_DOC
 integer :: i,j,k
 double precision :: hij
 integer :: degree(N_det),idx(0:N_det)
 call  i_H_j(psi_det(1,1,1),psi_det(1,1,1),N_int,hij)
 !$OMP PARALLEL DO SCHEDULE(GUIDED) PRIVATE(i,j,hij,degree,idx,k) &
 !$OMP SHARED (N_det, psi_det, N_int,H_matrix_all_dets)
 do i =1,N_det
! call get_excitation_degree_vector(psi_det,psi_det(1,1,i),degree,N_int,N_det,idx)
! do k =1, idx(0)
!  j = idx(k)
!  if(j.lt.i)cycle
   do j = i, N_det
   call  i_H_j(psi_det(1,1,i),psi_det(1,1,j),N_int,hij)
   H_matrix_all_dets(i,j) = hij
   H_matrix_all_dets(j,i) = hij
  enddo
 enddo
 !$OMP END PARALLEL DO
END_PROVIDER


subroutine provide_big_matrix_stored_with_current_dets(sze,dets_in,big_matrix_stored)
  use bitmasks
 integer, intent(in) :: sze
 integer(bit_kind), intent(in) :: dets_in(N_int,2,sze)
 double precision, intent(out) :: big_matrix_stored(sze,sze)
 integer :: i,j,k
 double precision :: hij
 integer :: degree(N_det),idx(0:N_det)
 call  i_H_j(dets_in(1,1,1),dets_in(1,1,1),N_int,hij)
 print*, 'providing big_matrix_stored'
 print*, n_det_max_stored
 !$OMP PARALLEL DO SCHEDULE(GUIDED) PRIVATE(i,j,hij,degree,idx,k) &
 !$OMP SHARED (sze, dets_in, N_int,big_matrix_stored)
 do i =1,sze
! call get_excitation_degree_vector(dets_in,dets_in(1,1,i),degree,N_int,sze,idx)
! do k =1, idx(0)
!  j = idx(k)
   do j = i, sze
   if(j.lt.i)cycle
   call  i_H_j(dets_in(1,1,i),dets_in(1,1,j),N_int,hij)
   big_matrix_stored(i,j) = hij
   big_matrix_stored(j,i) = hij
  enddo
 enddo
 !$OMP END PARALLEL DO
 print*, 'big_matrix_stored provided !!'


end
