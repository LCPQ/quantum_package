 BEGIN_PROVIDER [integer, pert_determinants, (N_states, psi_det_size) ]
 END_PROVIDER 


 BEGIN_PROVIDER [ double precision, lambda_mrcc, (N_states,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, lambda_pert, (N_states,psi_det_size) ] 
 implicit none
 BEGIN_DOC
 ! cm/<Psi_0|H|D_m> or perturbative 1/Delta_E(m)
 END_DOC
 integer :: i,k,j
 double precision               :: ihpsi(N_states), hii,delta_e_eff,ihpsi_current(N_states),hij
 integer :: i_ok,i_pert,i_pert_count
 i_ok = 0

 double precision :: phase_restart(N_states),tmp
 do k = 1, N_states
  phase_restart(k) = dsign(1.d0,psi_ref_coef_restart(1,k)/psi_ref_coef(1,k))
 enddo
 i_pert_count = 0
 
 do i=1,N_det_non_ref
   call i_h_psi(psi_non_ref(1,1,i), psi_ref_restart, psi_ref_coef_restart, N_int, N_det_ref,&
       size(psi_ref_coef_restart,1), n_states, ihpsi)
   call i_H_j(psi_non_ref(1,1,i),psi_non_ref(1,1,i),N_int,hii)
! TODO --- Test perturbatif  ------
   do k=1,N_states
     lambda_pert(k,i) = 1.d0 / (psi_ref_energy_diagonalized(k)-hii)
     ! TODO : i_h_psi peut sortir de la boucle?
     call i_h_psi(psi_non_ref(1,1,i), psi_ref, psi_ref_coef, N_int, N_det_ref,size(psi_ref_coef,1), n_states, ihpsi_current)
     if (ihpsi_current(k) == 0.d0) then
       ihpsi_current(k) = 1.d-32
     endif
     tmp = psi_non_ref_coef(i,k)/ihpsi_current(k)
     i_pert = 0
     ! Perturbation only if 1st order < 0.5 x second order
     if((ihpsi(k) * lambda_pert(k,i)) < 0.5d0 * psi_non_ref_coef_restart(i,k)  )then
       i_pert = 1
     else
       do j = 1, N_det_ref
         call i_H_j(psi_non_ref(1,1,i),psi_ref(1,1,j),N_int,hij)
         ! Perturbation diverges when hij*tmp > 0.5
         if(dabs(hij * tmp).ge.0.5d0)then
           i_pert_count +=1
           i_pert = 1
           exit
         endif
       enddo
     endif
     if( i_pert == 1)then
       pert_determinants(k,i) = i_pert
     endif
     if(pert_determinants(k,i) == 1)then
       i_ok +=1
       lambda_mrcc(k,i) = lambda_pert(k,i)
     else
       lambda_mrcc(k,i) = psi_non_ref_coef(i,k)/ihpsi_current(k)
     endif
   enddo
! TODO --- Fin test perturbatif ------
 enddo
!if(oscillations)then
! print*,'AVERAGING the lambda_mrcc with those of the previous iterations'
! do i = 1, N_det_non_ref
!  do k = 1, N_states

!   double precision :: tmp
!   tmp = lambda_mrcc(k,i)
!   lambda_mrcc(k,i) += lambda_mrcc_tmp(k,i)
!   lambda_mrcc(k,i) = lambda_mrcc(k,i) * 0.5d0
!   if(dabs(tmp - lambda_mrcc(k,i)).ge.1.d-9)then
!   print*,''
!   print*,'i = ',i
!   print*,'psi_non_ref_coef(i,k) = ',psi_non_ref_coef(i,k)
!   print*,'lambda_mrcc(k,i)     = ',lambda_mrcc(k,i)
!   print*,'                 tmp = ',tmp
!   endif
!  enddo
! enddo
!endif
 print*,'N_det_non_ref = ',N_det_non_ref
 print*,'Number of Perturbatively treated determinants = ',i_ok
 print*,'i_pert_count = ',i_pert_count
 print*,'psi_coef_ref_ratio = ',psi_ref_coef(2,1)/psi_ref_coef(1,1)

END_PROVIDER


BEGIN_PROVIDER [ double precision, lambda_mrcc_tmp, (N_states,psi_det_size) ]
 implicit none
 lambda_mrcc_tmp = 0.d0
END_PROVIDER 

BEGIN_PROVIDER [ logical, oscillations ]
 implicit none
 oscillations = .False.
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
&BEGIN_PROVIDER [ double precision, delta_cas, (N_det_ref,N_det_ref,N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 delta_ij = 0.d0
 delta_ii = 0.d0

 call H_apply_mrcc(delta_ij,delta_ii,N_det_ref,N_det_non_ref)
 double precision :: max_delta
 double precision :: accu
 integer :: imax,jmax
 max_delta = 0.d0
 accu = 0.d0
 do i = 1, N_det_ref
  do j = 1, N_det_non_ref
   accu += psi_non_ref_coef(j,1) * psi_ref_coef(i,1) * delta_ij(i,j,1)
   if(dabs(delta_ij(i,j,1)).gt.max_delta)then
    max_delta = dabs(delta_ij(i,j,1))
    imax = i
    jmax = j
   endif
  enddo
 enddo
 print*,''
 print*,''
 print*,'<psi| Delta H |psi> = ',accu
 print*,'MAX VAL OF DRESING = ',delta_ij(imax,jmax,1)
 print*,'imax,jmax = ',imax,jmax
 print*,'psi_ref_coef(imax,1)     = ',psi_ref_coef(imax,1)
 print*,'psi_non_ref_coef(jmax,1) = ',psi_non_ref_coef(jmax,1)
 do i = 1, N_det_ref
  print*,'delta_ii(i,1)     = ',delta_ii(i,1)
 enddo
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
   
   !!!!!!!!!!
   do ii = 1, N_det_ref
   do jj = 1, N_det_ref
    i = idx_ref(ii)
    j = idx_ref(jj)
    h_matrix_dressed(i,j,istate) += delta_cas(ii,jj,istate)
    h_matrix_dressed(j,i,istate) += delta_cas(ii,jj,istate)
   end do
   end do
   !!!!!!!!!!!!!
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
