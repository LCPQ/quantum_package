program mp2_wf
  implicit none
  BEGIN_DOC
! Save the MP2 wave function
  END_DOC
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, iter
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st), H_pert_diag(N_st))
  
  pt2 = 1.d0
  selection_criterion = 1.e-12
  selection_criterion_min = 1.e-12
  TOUCH selection_criterion_min selection_criterion selection_criterion_factor
  call H_apply_mp2_selection(pt2, norm_pert, H_pert_diag,  N_st)
  psi_det = psi_det_sorted
  psi_coef = psi_coef_sorted
  touch N_det psi_det psi_coef
  print*,'N_det = ',N_det
  print*,'-----'
  print *,  'PT2                          = ', pt2(1)
  print *,  'E                            = ', HF_energy
  print *,  'E_before     +PT2            = ', HF_energy+pt2(1)
  N_det = min(N_det,N_det_max)
  call save_wavefunction
  call ezfio_set_mp2_energy(HF_energy+pt2(1))
  deallocate(pt2,norm_pert,H_pert_diag)
end
