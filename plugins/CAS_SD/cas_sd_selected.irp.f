program full_ci
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  PROVIDE N_det_cas
  
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
    call H_apply_CAS_SD_selected(pt2, norm_pert, H_pert_diag,  N_st)

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
    call ezfio_set_cas_sd_energy(CI_energy(1))
    if (abort_all) then
      exit
    endif
  enddo
  call diagonalize_CI

   if(do_pt2_end)then
    print*,'Last iteration only to compute the PT2'
    threshold_selectors = 1.d0
    threshold_generators = 0.999d0
    call H_apply_CAS_SD_PT2(pt2, norm_pert, H_pert_diag,  N_st)

    print *,  'Final step'
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
    call ezfio_set_cas_sd_energy_pt2(CI_energy(1)+pt2(1))
   endif


  integer :: exc_max, degree_min
  exc_max = 0
  print *,  'CAS determinants : ', N_det_cas
  do i=1,min(N_det_cas,10)
    do k=i,N_det_cas
      call get_excitation_degree(psi_cas(1,1,k),psi_cas(1,1,i),degree,N_int)
      exc_max = max(exc_max,degree)
    enddo
    call debug_det(psi_cas(1,1,i),N_int)
    print *,  ''
  enddo
  print *,  'Max excitation degree in the CAS :', exc_max
end
