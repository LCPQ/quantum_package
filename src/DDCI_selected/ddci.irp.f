program full_ci
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  
  pt2 = 1.d0
  diag_algorithm = "Lapack"
  if (N_det > N_det_max) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = N_det_max
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
  endif

  do while (N_det < N_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    call H_apply_DDCI_selection(pt2, norm_pert, H_pert_diag,  N_st)

    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det > N_det_max) then
       psi_det = psi_det_sorted
       psi_coef = psi_coef_sorted
       N_det = N_det_max
       soft_touch N_det psi_det psi_coef
    endif
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
    call ezfio_set_full_ci_energy(CI_energy)
    if (abort_all) then
      exit
    endif
  enddo
  if(do_pt2_end)then
    call H_apply_DDCI_pt2(pt2, norm_pert, H_pert_diag,  N_st)
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
  endif
end
