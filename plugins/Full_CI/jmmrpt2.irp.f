program pouet
  implicit none
  integer                        :: i,k


  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  double precision :: i_H_psi_array(N_states),diag_H_mat_elem,h,i_O1_psi_array(N_states)
  double precision :: E_CI_before(N_states)
  integer :: n_det_before
  threshold_generators = threshold_generators_pt2
  threshold_selectors = threshold_selectors_pt2
  SOFT_TOUCH threshold_generators threshold_selectors
  call H_apply_FCI_PT2_new(pt2, norm_pert, H_pert_diag,  N_st)

  print *,  'Final step'
  print *,  'N_det    = ', N_det
  print *,  'N_states = ', N_states
  print *,  'PT2      = ', pt2
  print *,  'E        = ', CI_energy(1:N_states)
  print *,  'E+PT2    = ', CI_energy(1:N_states)+pt2(1:N_states)
  print *,  '-----'
  call ezfio_set_full_ci_energy_pt2(CI_energy(1)+pt2(1))
  deallocate(pt2,norm_pert)
end

