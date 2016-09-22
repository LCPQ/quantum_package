

subroutine run(N_st,energy)
  implicit none
  
  integer, intent(in) :: N_st 
  double precision, intent(out) :: energy(N_st) 

  integer :: i,j

  double precision :: E_new, E_old, delta_e
  integer :: iteration
  double precision :: E_past(4), lambda
  
  integer :: n_it_mrcc_max
  double precision :: thresh_mrcc
  

  
  thresh_mrcc = 1d-7
  n_it_mrcc_max = 10

  if(n_it_mrcc_max == 1) then
    do j=1,N_states_diag
      do i=1,N_det
        psi_coef(i,j) = CI_eigenvectors_dressed(i,j)
      enddo
    enddo
    SOFT_TOUCH psi_coef ci_energy_dressed
    call write_double(6,ci_energy_dressed(1),"Final MRCC energy")
    call ezfio_set_mrcc_cassd_energy(ci_energy_dressed(1))
    call save_wavefunction
    energy(:) = ci_energy_dressed(:)
  else
    E_new = 0.d0
    delta_E = 1.d0
    iteration = 0
    lambda = 1.d0
    do while (delta_E > thresh_mrcc)
      iteration += 1
      print *,  '===========================' 
      print *,  'MRCEPA0 Iteration', iteration
      print *,  '===========================' 
      print *,  ''
      E_old = sum(ci_energy_dressed)
      call write_double(6,ci_energy_dressed(1),"MRCEPA0 energy")
      call diagonalize_ci_dressed(lambda)
      E_new = sum(ci_energy_dressed)
      delta_E = dabs(E_new - E_old)
      call save_wavefunction
      call ezfio_set_mrcc_cassd_energy(ci_energy_dressed(1))
      if (iteration > n_it_mrcc_max) then
        exit
      endif
    enddo
    call write_double(6,ci_energy_dressed(1),"Final MRCEPA0 energy")
    energy(:) = ci_energy_dressed(:)
  endif
end


subroutine print_cas_coefs
  implicit none

  integer :: i,j
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  psi_cas_coef(i,:)
    call debug_det(psi_cas(1,1,i),N_int)
  enddo
  call write_double(6,ci_energy(1),"Initial CI energy")

end




subroutine run_pt2_old(N_st,energy) 
  implicit none 
  integer :: i,j,k 
  integer, intent(in)          :: N_st 
  double precision, intent(in) :: energy(N_st) 
  double precision :: pt2_redundant(N_st), pt2(N_st)
  double precision :: norm_pert(N_st),H_pert_diag(N_st)
  
  pt2_redundant = 0.d0 
  pt2 = 0d0
  !if(lambda_mrcc_pt2(0)  == 0) return
  
  print*,'Last iteration only to compute the PT2' 
  
  print * ,'Computing the redundant PT2 contribution'

  if (mrmode == 1) then

    N_det_generators = lambda_mrcc_kept(0) 
    N_det_selectors = lambda_mrcc_kept(0) 

    do i=1,N_det_generators
      j = lambda_mrcc_kept(i)
      do k=1,N_int
        psi_det_generators(k,1,i) = psi_non_ref(k,1,j)
        psi_det_generators(k,2,i) = psi_non_ref(k,2,j)
        psi_selectors(k,1,i) = psi_non_ref(k,1,j)
        psi_selectors(k,2,i) = psi_non_ref(k,2,j)
      enddo
      do k=1,N_st
        psi_coef_generators(i,k) = psi_non_ref_coef(j,k)
        psi_selectors_coef(i,k) = psi_non_ref_coef(j,k)
      enddo
    enddo

  else

    N_det_generators = N_det_non_ref
    N_det_selectors = N_det_non_ref

    do i=1,N_det_generators
      j = i
      do k=1,N_int
        psi_det_generators(k,1,i) = psi_non_ref(k,1,j)
        psi_det_generators(k,2,i) = psi_non_ref(k,2,j)
        psi_selectors(k,1,i) = psi_non_ref(k,1,j)
        psi_selectors(k,2,i) = psi_non_ref(k,2,j)
      enddo
      do k=1,N_st
        psi_coef_generators(i,k) = psi_non_ref_coef(j,k)
        psi_selectors_coef(i,k) = psi_non_ref_coef(j,k)
      enddo
    enddo

  endif
  
  SOFT_TOUCH N_det_selectors psi_selectors_coef psi_selectors N_det_generators psi_det_generators psi_coef_generators ci_eigenvectors_dressed ci_eigenvectors_s2_dressed ci_electronic_energy_dressed
  SOFT_TOUCH psi_ref_coef_diagonalized psi_ref_energy_diagonalized

  call H_apply_mrcepa_PT2(pt2_redundant, norm_pert, H_pert_diag,  N_st) 
  
  print * ,'Computing the remaining contribution'

  threshold_selectors = 1.d0 
  threshold_generators = 0.999d0 

  N_det_generators = N_det_non_ref + N_det_ref
  N_det_selectors = N_det_non_ref + N_det_ref
  
  psi_det_generators(:,:,:N_det_ref) = psi_ref(:,:,:N_det_ref)
  psi_selectors(:,:,:N_det_ref) = psi_ref(:,:,:N_det_ref)
  psi_coef_generators(:N_det_ref,:) = psi_ref_coef(:N_det_ref,:)
  psi_selectors_coef(:N_det_ref,:) = psi_ref_coef(:N_det_ref,:)
  
  do i=N_det_ref+1,N_det_generators
    j = i-N_det_ref
    do k=1,N_int
      psi_det_generators(k,1,i) = psi_non_ref(k,1,j)
      psi_det_generators(k,2,i) = psi_non_ref(k,2,j)
      psi_selectors(k,1,i) = psi_non_ref(k,1,j)
      psi_selectors(k,2,i) = psi_non_ref(k,2,j)
    enddo
    do k=1,N_st
      psi_coef_generators(i,k) = psi_non_ref_coef(j,k)
      psi_selectors_coef(i,k) = psi_non_ref_coef(j,k)
    enddo
  enddo
  
  SOFT_TOUCH N_det_selectors psi_selectors_coef psi_selectors N_det_generators psi_det_generators psi_coef_generators ci_eigenvectors_dressed ci_eigenvectors_s2_dressed ci_electronic_energy_dressed
  SOFT_TOUCH psi_ref_coef_diagonalized psi_ref_energy_diagonalized

  call H_apply_mrcepa_PT2(pt2, norm_pert, H_pert_diag,  N_st) 
 

  print *, "Redundant PT2 :",pt2_redundant
  print *, "Full      PT2 :",pt2
  print *, lambda_mrcc_kept(0), N_det, N_det_ref, psi_coef(1,1), psi_ref_coef(1,1)
  pt2 = pt2 - pt2_redundant
  
  print *,  'Final step' 
  print *,  'N_det    = ', N_det 
  print *,  'N_states = ', N_states 
  print *,  'PT2      = ', pt2 
  print *,  'E        = ', energy 
  print *,  'E+PT2    = ', energy+pt2 
  print *,  '-----' 
 

!  call ezfio_set_full_ci_energy_pt2(energy+pt2)

end 

subroutine run_pt2(N_st,energy) 
  implicit none 
  integer :: i,j,k 
  integer, intent(in)          :: N_st 
  double precision, intent(in) :: energy(N_st) 
  double precision :: pt2(N_st)
  double precision :: norm_pert(N_st),H_pert_diag(N_st)
  
  pt2 = 0d0
  !if(lambda_mrcc_pt2(0)  == 0) return
  
  print*,'Last iteration only to compute the PT2' 
  
  N_det_generators = N_det_cas
  N_det_selectors = N_det_non_ref

  do i=1,N_det_generators
    do k=1,N_int
      psi_det_generators(k,1,i) = psi_ref(k,1,i)
      psi_det_generators(k,2,i) = psi_ref(k,2,i)
    enddo
    do k=1,N_st
      psi_coef_generators(i,k) = psi_ref_coef(i,k)
    enddo
  enddo
  do i=1,N_det
    do k=1,N_int
      psi_selectors(k,1,i) = psi_det_sorted(k,1,i)
      psi_selectors(k,2,i) = psi_det_sorted(k,2,i)
    enddo
    do k=1,N_st
      psi_selectors_coef(i,k) = psi_coef_sorted(i,k)
    enddo
  enddo

  SOFT_TOUCH N_det_selectors psi_selectors_coef psi_selectors N_det_generators psi_det_generators psi_coef_generators ci_eigenvectors_dressed ci_eigenvectors_s2_dressed ci_electronic_energy_dressed
  SOFT_TOUCH psi_ref_coef_diagonalized psi_ref_energy_diagonalized

  call H_apply_mrcepa_PT2(pt2, norm_pert, H_pert_diag,  N_st) 
  
!  call ezfio_set_full_ci_energy_pt2(energy+pt2)

  print *,  'Final step' 
  print *,  'N_det    = ', N_det 
  print *,  'N_states = ', N_states 
  print *,  'PT2      = ', pt2 
  print *,  'E        = ', energy 
  print *,  'E+PT2    = ', energy+pt2 
  print *,  '-----' 

end 

