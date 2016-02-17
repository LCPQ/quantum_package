 BEGIN_PROVIDER [double precision, psi_ref_coef_dressed, (n_det_ref,N_states) ]
&BEGIN_PROVIDER [double precision, energies_ref_dressed, (N_states) ]
 implicit none
 integer :: i,j,k,l,istate,igoodstate
 double precision, allocatable :: H_matrix_tmp(:,:)
 double precision, allocatable :: eigvalues(:),eigvectors(:,:),psi_coef_ref_tmp(:)
 double precision :: accu, accu1
 allocate(H_matrix_tmp(n_det_ref,n_det_ref))
 allocate(eigvalues(n_det_ref))
 allocate(eigvectors(n_det_ref,n_det_ref))
 allocate(psi_coef_ref_tmp(n_det_ref))
 do istate = 1, N_states
  accu1 = 0.d0
  do j = 1, n_det_ref
   accu1 += psi_ref_coef(j,istate)**2   ! norm of the "istate" eigenvector in the projected in the reference space
   do k = 1, n_det_ref
    H_matrix_tmp(j,k) = hamiltonian_total_dressed(j,k,istate)
   enddo
  enddo
  accu1 = 1.d0/dsqrt(accu1)
  do j = 1, n_det_ref
   psi_coef_ref_tmp(j) = psi_ref_coef(j,istate) * accu1
  enddo
  call lapack_diagd(eigvalues,eigvectors,H_matrix_tmp,n_det_ref,n_det_ref)
  do j = 1, n_det_ref
   accu = 0.d0
   do k = 1, n_det_ref 
    accu += eigvectors(k,j) * psi_coef_ref_tmp(k)
   enddo
   if(dabs(accu).gt.0.9d0)then
    igoodstate = j
    exit
   endif
  enddo
  energies_ref_dressed(istate) = eigvalues(igoodstate)
  do j = 1,n_det_ref
   psi_ref_coef_dressed(j,istate) = eigvectors(j,igoodstate)
  enddo
 enddo

END_PROVIDER 
