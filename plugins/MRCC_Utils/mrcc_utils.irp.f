 BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states,psi_det_size) ]
&BEGIN_PROVIDER [ integer, lambda_mrcc_pt2, (0:psi_det_size) ]
  implicit none
  BEGIN_DOC
  ! cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
  END_DOC
  integer :: i,k
  double precision               :: ihpsi_current(N_states)
  integer                        :: i_pert_count
  double precision               :: hii, lambda_pert
  integer                        :: N_lambda_mrcc_pt2

  i_pert_count = 0
  lambda_mrcc = 0.d0
  N_lambda_mrcc_pt2 = 0
  lambda_mrcc_pt2(0) = 0

    do i=1,N_det_non_ref
      call i_h_psi(psi_non_ref(1,1,i), psi_ref, psi_ref_coef_normalized, N_int, N_det_ref,&
          size(psi_ref_coef,1), N_states,ihpsi_current)
      call i_H_j(psi_non_ref(1,1,i),psi_non_ref(1,1,i),N_int,hii)
      do k=1,N_states
        if (ihpsi_current(k) == 0.d0) then
          ihpsi_current(k) = 1.d-32
        endif
        lambda_mrcc(k,i) = min(0.d0,psi_non_ref_coef(i,k)/ihpsi_current(k) )
        lambda_pert = 1.d0 / (psi_ref_energy_diagonalized(k)-hii)
        if (lambda_pert / lambda_mrcc(k,i)  < 0.5d0) then
          i_pert_count += 1
          lambda_mrcc(k,i) = 0.d0
          if (lambda_mrcc_pt2(N_lambda_mrcc_pt2) /= i) then
            N_lambda_mrcc_pt2 += 1
            lambda_mrcc_pt2(N_lambda_mrcc_pt2) = i
          endif
        endif
      enddo
    enddo
    lambda_mrcc_pt2(0) = N_lambda_mrcc_pt2
  
  print*,'N_det_non_ref = ',N_det_non_ref
  print*,'Number of ignored determinants = ',i_pert_count  
  print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)
  print*,'lambda max = ',maxval(dabs(lambda_mrcc))

END_PROVIDER







BEGIN_PROVIDER [ double precision, hij_mrcc, (N_det_non_ref,N_det_ref) ]
 implicit none
 BEGIN_DOC
 ! < ref | H | Non-ref > matrix
 END_DOC
 integer :: i_I, k_sd
  do i_I=1,N_det_ref
    do k_sd=1,N_det_non_ref
      call i_h_j(psi_ref(1,1,i_I),psi_non_ref(1,1,k_sd),N_int,hij_mrcc(k_sd,i_I))
    enddo
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, delta_ij, (N_states,N_det_non_ref,N_det_ref) ]
&BEGIN_PROVIDER [ double precision, delta_ii, (N_states,N_det_ref) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 delta_ij = 0.d0
 delta_ii = 0.d0
 call H_apply_mrcc(delta_ij,delta_ii,N_states,N_det_non_ref,N_det_ref)
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
     h_matrix_dressed(i,i,istate) += delta_ii(istate,ii)
    do jj = 1, N_det_non_ref
     j =idx_non_ref(jj)
     h_matrix_dressed(i,j,istate) += delta_ij(istate,jj,ii)
     h_matrix_dressed(j,i,istate) += delta_ij(istate,jj,ii)
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

subroutine diagonalize_CI_dressed(lambda)
  implicit none
  BEGIN_DOC
!  Replace the coefficients of the CI states by the coefficients of the 
!  eigenstates of the CI matrix
  END_DOC
  double precision, intent(in) :: lambda
  integer :: i,j
  do j=1,N_states_diag
    do i=1,N_det
      psi_coef(i,j) = lambda * CI_eigenvectors_dressed(i,j) + (1.d0 - lambda) * psi_coef(i,j) 
    enddo
    call normalize(psi_coef(1,j), N_det)
  enddo
  SOFT_TOUCH psi_coef 

end

