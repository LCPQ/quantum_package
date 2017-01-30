 BEGIN_PROVIDER [double precision, CI_eigenvectors_sc2_no_amp, (N_det,N_states_diag)]
&BEGIN_PROVIDER [double precision, CI_eigenvectors_s2_sc2_no_amp, (N_states_diag)]
&BEGIN_PROVIDER [double precision, CI_electronic_energy_sc2_no_amp, (N_states_diag)]
   implicit none
   integer                        :: i,j,k,l
   integer, allocatable           :: idx(:)
   integer, allocatable           :: holes_part(:,:)
   double precision, allocatable  :: e_corr(:,:)
   double precision, allocatable  :: accu(:)
   double precision, allocatable  :: ihpsi_current(:)
   double precision, allocatable  :: H_jj(:),H_jj_total(:),S2_jj(:)
   integer :: number_of_particles, number_of_holes, n_h,n_p
   allocate(e_corr(N_det_non_ref,N_states),ihpsi_current(N_states),accu(N_states),H_jj(N_det_non_ref),idx(0:N_det_non_ref))
   allocate(H_jj_total(N_det),S2_jj(N_det))
   allocate(holes_part(N_det,2))
     accu = 0.d0
     do i = 1, N_det_non_ref
       holes_part(i,1) = number_of_holes(psi_non_ref(1,1,i))
       holes_part(i,2) = number_of_particles(psi_non_ref(1,1,i))
       call i_h_psi(psi_non_ref(1,1,i), psi_ref, psi_ref_coef, N_int, N_det_ref,&
           size(psi_ref_coef_interm_norm,1), N_states,ihpsi_current)
       do j = 1, N_states
         e_corr(i,j) = psi_non_ref_coef(i,j) * ihpsi_current(j) * inv_norm_psi_ref(j)
         accu(j) +=  e_corr(i,j)
       enddo
     enddo
     print *, 'accu = ',accu
     double precision               :: hjj,diag_h_mat_elem
     do i = 1, N_det_non_ref
       H_jj(i) = 0.d0
       n_h = holes_part(i,1)
       n_p = holes_part(i,2)
       integer :: degree
!        do j = 1, N_det_non_ref
!         call get_excitation_degree(psi_non_ref(1,1,i),psi_non_ref(1,1,j),degree,N_int)
!         if(degree .gt. 2)then
!          if(n_h + holes_part(j,1) .gt. 2 .or. n_p + holes_part(j,2) .gt. 2 ) then
!           H_jj(i) += e_corr(j,1)
!          endif
!         endif
!        enddo
       call filter_not_connected(psi_non_ref,psi_non_ref(1,1,i),N_int,N_det_non_ref,idx)
       do j = 1, idx(0)
         if(n_h + holes_part(idx(j),1) .gt. 2 .or. n_p + holes_part(idx(j),2) .gt. 2 ) then
          H_jj(i) += e_corr(idx(j),1)
         endif
       enddo
     enddo

     do i=1,N_Det
       H_jj_total(i) = diag_h_mat_elem(psi_det(1,1,i),N_int)
       call get_s2(psi_det(1,1,i),psi_det(1,1,i),N_int,S2_jj(i))
     enddo
     do i = 1, N_det_non_ref
       H_jj_total(idx_non_ref(i)) += H_jj(i)
     enddo
     
     
     print *,  'coef'
     call davidson_diag_hjj_sjj(psi_det,CI_eigenvectors_sc2_no_amp,H_jj_total,S2_jj,CI_electronic_energy_sc2_no_amp,size(CI_eigenvectors_sc2_no_amp,1),N_Det,N_states,N_states_diag,N_int,6)
     do i = 1, N_det
       hjj = diag_h_mat_elem(psi_det(1,1,i),N_int)
     ! if(hjj<-210.d0)then
     !  call debug_det(psi_det(1,1,i),N_int)
     !  print *, CI_eigenvectors_sc2_no_amp((i),1),hjj, H_jj_total(i)
     ! endif
     enddo





     print *,  'ref',N_det_ref
     do i =1, N_det_ref
        call debug_det(psi_det(1,1,idx_ref(i)),N_int)
      print *,  CI_eigenvectors_sc2_no_amp(idx_ref(i),1), H_jj_total(idx_ref(i))  
     enddo
     print *,  'non ref',N_det_non_ref
     do i=1, N_det_non_ref
       hjj = diag_h_mat_elem(psi_non_ref(1,1,i),N_int)
!      print *, CI_eigenvectors_sc2_no_amp(idx_non_ref(i),1),H_jj_total(idx_non_ref(i)), H_jj(i)
!      if(dabs(CI_eigenvectors_sc2_no_amp(idx_non_ref(i),1)).gt.1.d-1)then
!      if(hjj<-210.d0)then
!       call debug_det(psi_det(1,1,idx_non_ref(i)),N_int)
!       write(*,'(10(F16.10,X))') CI_eigenvectors_sc2_no_amp(idx_non_ref(i),1),hjj, H_jj(i),H_jj_total(idx_non_ref(i))
!      endif
     enddo
!    do i = 1, N_det
!     print *,  CI_eigenvectors_sc2_no_amp(i,1)
!    enddo
     do i=1,N_states_diag
       CI_eigenvectors_s2_sc2_no_amp(i) = S2_jj(i)
     enddo
     
   deallocate(e_corr,ihpsi_current,accu,H_jj,idx,H_jj_total,s2_jj,holes_part)
END_PROVIDER 

BEGIN_PROVIDER [ double precision, CI_energy_sc2_no_amp, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_determinants)
  do j=1,min(N_det,N_states_diag)
    CI_energy_sc2_no_amp(j) = CI_electronic_energy_sc2_no_amp(j) + nuclear_repulsion
  enddo
  do j=1,min(N_det,N_states)
    write(st,'(I4)') j
    call write_double(output_determinants,CI_energy_sc2_no_amp(j),'Energy of state '//trim(st))
    call write_double(output_determinants,CI_eigenvectors_s2_sc2_no_amp(j),'S^2 of state '//trim(st))
  enddo

END_PROVIDER

subroutine diagonalize_CI_sc2_no_amp
 implicit none
  integer :: i,j
  do j=1,N_states
    do i=1,N_det
      psi_coef(i,j) = CI_eigenvectors_sc2_no_amp(i,j)
    enddo
  enddo
  SOFT_TOUCH  ci_eigenvectors_s2_sc2_no_amp ci_eigenvectors_sc2_no_amp ci_electronic_energy_sc2_no_amp ci_energy_sc2_no_amp psi_coef 

end
  
