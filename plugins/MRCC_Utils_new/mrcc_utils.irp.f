 BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, lambda_pert, (N_states,psi_det_size) ] 
 implicit none
 BEGIN_DOC
 ! cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
 END_DOC
 integer :: i,k
 double precision               :: ihpsi(N_states), hii
 integer :: i_ok
 i_ok = 0
 
 do i=1,N_det_non_ref
   call i_h_psi(psi_non_ref(1,1,i), psi_ref, psi_ref_coef, N_int, N_det_ref,&
       size(psi_ref_coef,1), n_states, ihpsi)
   call i_h_j(psi_non_ref(1,1,i),psi_non_ref(1,1,i),N_int,hii)
   do k=1,N_states
     lambda_pert(k,i) = 1.d0 / (psi_ref_energy_diagonalized(k)-hii)
     if (dabs(ihpsi(k)).le.1.d-3) then
       i_ok +=1
       lambda_mrcc(k,i) = lambda_pert(k,i)
     else
       lambda_mrcc(k,i) = psi_non_ref_coef(i,k)/ihpsi(k)
     endif
   enddo
 enddo
 print*,'N_det_non_ref = ',N_det_non_ref
 print*,'Number of Perturbatively treated determinants = ',i_ok
 print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)

END_PROVIDER




!BEGIN_PROVIDER [ double precision, delta_ij_non_ref, (N_det_non_ref, N_det_non_ref,N_states) ]
!implicit none
!BEGIN_DOC
!! Dressing matrix in SD basis
!END_DOC
!delta_ij_non_ref = 0.d0
!call H_apply_mrcc_simple(delta_ij_non_ref,N_det_non_ref)
!END_PROVIDER

 BEGIN_PROVIDER [ double precision, delta_ij, (N_det_ref,N_det_non_ref,N_states) ]
&BEGIN_PROVIDER [ double precision, delta_ii, (N_det_ref,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 delta_ij = 0.d0
 delta_ii = 0.d0
 call mrcc_dress(N_det_ref,N_det_non_ref,N_states,delta_ij,delta_ii)
 write(33,*)delta_ij
 write(34,*)delta_ii
END_PROVIDER

BEGIN_PROVIDER [ double precision, h_matrix_dressed, (N_det,N_det,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressed H with Delta_ij
 END_DOC
 integer                        :: i, j,istate,ii,jj
 do istate = 1,N_states
   do j=1,N_det
     do i=1,N_det
       h_matrix_dressed(i,j,istate) = h_matrix_all_dets(i,j) 
     enddo
   enddo
   do ii = 1, N_det_ref
     i =idx_ref(ii)
     h_matrix_dressed(i,i,istate) += delta_ii(ii,istate)
    do jj = 1, N_det_non_ref
     j =idx_non_ref(jj)
     h_matrix_dressed(i,j,istate) += delta_ij(ii,jj,istate)
     h_matrix_dressed(j,i,istate) += delta_ij(ii,jj,istate)
    enddo
   enddo 
 enddo
END_PROVIDER


 BEGIN_PROVIDER [ double precision, CI_electronic_energy_dressed, (N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_dressed, (N_det,N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_s2_dressed, (N_states_diag) ]
   implicit none
   BEGIN_DOC
   ! Eigenvectors/values of the CI matrix
   END_DOC
   integer                        :: i,j
   
   do j=1,N_states_diag
     do i=1,N_det
       CI_eigenvectors_dressed(i,j) = psi_coef(i,j)
     enddo
   enddo
   
   if (diag_algorithm == "Davidson") then
     
     integer                        :: istate
     istate = 1
     call davidson_diag_mrcc(psi_det,CI_eigenvectors_dressed,CI_electronic_energy_dressed,&
         size(CI_eigenvectors_dressed,1),N_det,N_states_diag,N_int,output_determinants,istate)
     
   else if (diag_algorithm == "Lapack") then
     
     double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
     allocate (eigenvectors(size(H_matrix_dressed,1),N_det))
     allocate (eigenvalues(N_det))
     call lapack_diag(eigenvalues,eigenvectors,                      &
         H_matrix_dressed,size(H_matrix_dressed,1),N_det)
     CI_electronic_energy_dressed(:) = 0.d0
     do i=1,N_det
       CI_eigenvectors_dressed(i,1) = eigenvectors(i,1)
     enddo
     integer                        :: i_state
     double precision               :: s2
     i_state = 0
     if (s2_eig) then
       do j=1,N_det
         call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
         if(dabs(s2-expected_s2).le.0.3d0)then
           i_state += 1
           do i=1,N_det
             CI_eigenvectors_dressed(i,i_state) = eigenvectors(i,j)
           enddo
           CI_electronic_energy_dressed(i_state) = eigenvalues(j)
           CI_eigenvectors_s2_dressed(i_state) = s2
         endif
         if (i_state.ge.N_states_diag) then
           exit
         endif
       enddo
     else
       do j=1,N_states_diag
         call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
         i_state += 1
         do i=1,N_det
           CI_eigenvectors_dressed(i,i_state) = eigenvectors(i,j)
         enddo
         CI_electronic_energy_dressed(i_state) = eigenvalues(j)
         CI_eigenvectors_s2_dressed(i_state) = s2
       enddo
     endif
     deallocate(eigenvectors,eigenvalues)
   endif
   
END_PROVIDER

BEGIN_PROVIDER [ double precision, CI_energy_dressed, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the dressed CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_determinants)
  do j=1,N_states_diag
    CI_energy_dressed(j) = CI_electronic_energy_dressed(j) + nuclear_repulsion
  enddo

END_PROVIDER

subroutine diagonalize_CI_dressed
  implicit none
  BEGIN_DOC
!  Replace the coefficients of the CI states by the coefficients of the 
!  eigenstates of the CI matrix
  END_DOC
  integer :: i,j
  do j=1,N_states_diag
    do i=1,N_det
      psi_coef(i,j) = CI_eigenvectors_dressed(i,j)
    enddo
  enddo
  SOFT_TOUCH psi_coef 

end
