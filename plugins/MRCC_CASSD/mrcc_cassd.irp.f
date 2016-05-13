program mrcc
  implicit none
  double precision, allocatable  :: energy(:)
  allocate (energy(N_states))
  
  read_wf = .True.
  SOFT_TOUCH read_wf
  call print_cas_coefs
  call set_generators_bitmasks_as_holes_and_particles
  call run(N_states,energy)
  if(do_pt2_end)then
    call run_pt2(N_states,energy)
  endif
  deallocate(energy)
end

subroutine run(N_st,energy)
  implicit none
  
  integer, intent(in) :: N_st 
  double precision, intent(out) :: energy(N_st) 

  integer :: i

  double precision :: E_new, E_old, delta_e
  integer :: iteration
  double precision :: E_past(4), lambda
  E_new = 0.d0
  delta_E = 1.d0
  iteration = 0
  lambda = 1.d0
  do while (delta_E > thresh_mrcc)
    iteration += 1
    print *,  '===========================' 
    print *,  'MRCC Iteration', iteration
    print *,  '===========================' 
    print *,  ''
    E_old = sum(ci_energy_dressed)
    call write_double(6,ci_energy_dressed(1),"MRCC energy")
    call diagonalize_ci_dressed(lambda)
    E_new = sum(ci_energy_dressed)
    delta_E = dabs(E_new - E_old)
    call save_wavefunction
    call ezfio_set_mrcc_cassd_energy(ci_energy_dressed(1))
    if (iteration > n_it_mrcc_max) then
      exit
    endif
  enddo
  call write_double(6,ci_energy_dressed(1),"Final MRCC energy")
  energy(:) = ci_energy_dressed(:)

end


subroutine run_pt2(N_st,energy) 
  implicit none 
  integer :: i,j,k 
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:) 
  integer, intent(in)          :: N_st 
  double precision, intent(in) :: energy(N_st) 
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st)) 
  pt2 = 0.d0 
 
  print*,'Last iteration only to compute the PT2' 
  threshold_selectors = 1.d0 
  threshold_generators = 0.999d0 
 
  N_det_generators = lambda_mrcc_pt2(0) + N_det_cas
  do i=1,N_det_cas
    do k=1,N_int 
      psi_det_generators(k,1,i) = psi_ref(k,1,i) 
      psi_det_generators(k,2,i) = psi_ref(k,2,i) 
    enddo 
    do k=1,N_st 
      psi_coef_generators(i,k) = psi_ref_coef(i,k) 
    enddo 
  enddo 
  do i=N_det_cas+1,N_det_generators
    j = lambda_mrcc_pt2(i)  
    do k=1,N_int 
      psi_det_generators(k,1,i) = psi_non_ref(k,1,j) 
      psi_det_generators(k,2,i) = psi_non_ref(k,2,j) 
    enddo 
    do k=1,N_st 
      psi_coef_generators(i,k) = psi_non_ref_coef(j,k) 
    enddo 
  enddo 
  SOFT_TOUCH N_det_generators psi_det_generators psi_coef_generators ci_eigenvectors_dressed ci_eigenvectors_s2_dressed ci_electronic_energy_dressed 
 
 
  call H_apply_mrcc_PT2(pt2, norm_pert, H_pert_diag,  N_st) 
  print *,  'Final step' 
  print *,  'N_det    = ', N_det 
  print *,  'N_states = ', N_states 
  print *,  'PT2      = ', pt2 
  print *,  'E        = ', energy 
  print *,  'E+PT2    = ', energy+pt2 
  print *,  '-----' 
 

  call ezfio_set_full_ci_energy_pt2(energy+pt2)
  deallocate(pt2,norm_pert)

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

