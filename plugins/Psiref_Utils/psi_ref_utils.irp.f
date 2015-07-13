use bitmasks


 BEGIN_PROVIDER [ integer(bit_kind), psi_ref_sorted_bit, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_ref_coef_sorted_bit, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! Reference determinants sorted to accelerate the search of a random determinant in the wave
 ! function.
 END_DOC
 call sort_dets_by_det_search_key(N_det_ref, psi_ref, psi_ref_coef, &
     psi_ref_sorted_bit, psi_ref_coef_sorted_bit)

END_PROVIDER



 BEGIN_PROVIDER [ integer(bit_kind), psi_non_ref,  (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_non_ref_coef, (psi_det_size,n_states) ]
&BEGIN_PROVIDER [ integer, idx_non_ref,  (psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_det_non_ref ]
 implicit none
 BEGIN_DOC
  ! Set of determinants which are not part of the reference, defined from the application
  ! of the reference bitmask on the determinants. 
  ! idx_non_ref gives the indice of the determinant in psi_det.
 END_DOC
 integer                        :: i_non_ref,j,k
 integer                        :: degree
 logical                        :: in_ref
 i_non_ref =0
 do k=1,N_det
   in_ref = .False.
   do j=1,N_det_ref
     call get_excitation_degree(psi_ref(1,1,j), psi_det(1,1,k), degree, N_int)
     if (degree == 0) then
       in_ref = .True.
       exit
     endif
   enddo
   if (.not.in_ref) then
     double precision :: hij
     i_non_ref += 1
     do j=1,N_int
       psi_non_ref(j,1,i_non_ref) = psi_det(j,1,k)
       psi_non_ref(j,2,i_non_ref) = psi_det(j,2,k)
     enddo
     do j=1,N_states
       psi_non_ref_coef(i_non_ref,j) = psi_coef(k,j)
     enddo
     idx_non_ref(i_non_ref) = k
   endif
 enddo
 N_det_non_ref = i_non_ref
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_non_ref_sorted_bit, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_non_ref_coef_sorted_bit, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! Reference determinants sorted to accelerate the search of a random determinant in the wave
 ! function.
 END_DOC
 call sort_dets_by_det_search_key(N_det_ref, psi_non_ref, psi_non_ref_coef, &
     psi_non_ref_sorted_bit, psi_non_ref_coef_sorted_bit)

END_PROVIDER


BEGIN_PROVIDER [double precision, H_matrix_ref, (N_det_ref,N_det_ref)] 
 implicit none
 integer :: i,j
 double precision :: hij
  do i = 1, N_det_ref
   do j = 1, N_det_ref
    call i_H_j(psi_ref(1,1,i),psi_ref(1,1,j),N_int,hij) 
    H_matrix_ref(i,j) = hij
   enddo
  enddo
END_PROVIDER

 BEGIN_PROVIDER [double precision, psi_coef_ref_diagonalized, (N_det_ref,N_states)]
&BEGIN_PROVIDER [double precision, psi_ref_energy_diagonalized, (N_states)]
 implicit none
 integer :: i,j
  double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
  allocate (eigenvectors(size(H_matrix_ref,1),N_det_ref))
  allocate (eigenvalues(N_det_ref))
  call lapack_diag(eigenvalues,eigenvectors,                       &
      H_matrix_ref,size(H_matrix_ref,1),N_det_ref)
  do i = 1, N_states
   psi_ref_energy_diagonalized(i) = eigenvalues(i)
   do j = 1, N_det_ref
    psi_coef_ref_diagonalized(j,i) = eigenvectors(j,i)
   enddo
  enddo


 END_PROVIDER

 BEGIN_PROVIDER [double precision, psi_ref_energy, (N_states)]
 implicit none
 integer :: i,j,k
 double precision :: hij,norm,u_dot_v
  psi_ref_energy = 0.d0


  do k = 1, N_states
   norm = 0.d0
   do i = 1, N_det_ref
    norm += psi_ref_coef(i,k) * psi_ref_coef(i,k)
    do j = 1, N_det_ref
      psi_ref_energy(k) += psi_ref_coef(i,k) * psi_ref_coef(j,k) * H_matrix_ref(i,j)
    enddo
   enddo
   psi_ref_energy(k) = psi_ref_energy(k) /norm
  enddo

END_PROVIDER 




