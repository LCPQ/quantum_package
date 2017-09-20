

subroutine run(N_st,energy)
  implicit none
  
  integer, intent(in) :: N_st 
  double precision, intent(out) :: energy(N_st) 

  integer :: i,j

  double precision :: E_new, E_old, delta_e
  integer :: iteration
  double precision :: E_past(4)
  
  integer :: n_it_mrcc_max
  double precision :: thresh_mrcc
  double precision, allocatable :: lambda(:)
  allocate (lambda(N_states))

  thresh_mrcc = thresh_dressed_ci
  n_it_mrcc_max = n_it_max_dressed_ci

  if(n_it_mrcc_max == 1) then
    do j=1,N_states
      do i=1,N_det
        psi_coef(i,j) = CI_eigenvectors_dressed(i,j)
      enddo
    enddo
    SOFT_TOUCH psi_coef ci_energy_dressed
    call write_double(6,ci_energy_dressed(1),"Final MRCC energy")
    call ezfio_set_mrcepa0_energy(ci_energy_dressed(1))
    call save_wavefunction
  else
    E_new = 0.d0
    delta_E = 1.d0
    iteration = 0
    lambda = 1.d0
    do while (delta_E > thresh_mrcc)
      iteration += 1
      print *,  '===============================================' 
      print *,  'Iteration', iteration, '/', n_it_mrcc_max
      print *,  '===============================================' 
      print *,  ''
      E_old = sum(ci_energy_dressed(1:N_states))
      do i=1,N_st
        call write_double(6,ci_energy_dressed(i),"Energy")
      enddo
      call diagonalize_ci_dressed(lambda)
      E_new = sum(ci_energy_dressed(1:N_states))
!  if (.true.) then
!    provide delta_ij_mrcc_pouet
!  endif
      delta_E = (E_new - E_old)/dble(N_states)
      print *,  ''
      call write_double(6,thresh_mrcc,"thresh_mrcc")
      call write_double(6,delta_E,"delta_E")
      delta_E = dabs(delta_E)
      call save_wavefunction
      call ezfio_set_mrcepa0_energy(ci_energy_dressed(1))
      if (iteration >= n_it_mrcc_max) then
        exit
      endif
    enddo
    call write_double(6,ci_energy_dressed(1),"Final energy")
  endif
  energy(1:N_st) = ci_energy_dressed(1:N_st)
end


subroutine print_cas_coefs
  implicit none

  integer :: i,j
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  (psi_cas_coef(i,j), j=1,N_states)
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

  threshold_selectors = max(threshold_selectors,threshold_selectors_pt2)
  threshold_generators = max(threshold_generators,threshold_generators_pt2)

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
 

  call ezfio_set_mrcepa0_energy_pt2(energy(1)+pt2(1))

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

  call ezfio_set_mrcepa0_energy_pt2(energy(1)+pt2(1))

end 

