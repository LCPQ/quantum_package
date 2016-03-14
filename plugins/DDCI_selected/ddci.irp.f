program ddci
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:),E_before(:)
  integer                        :: N_st, degree
  N_st = N_states_diag
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st),E_before(N_st))
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
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_bitmask_hole_as_input(reunion_of_bitmask)

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
    print *,  'E+PT2    = ', E_before+pt2
    print *,  '-----'
    if(N_states_diag.gt.1)then
     print*,'Variational Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',CI_energy(i) - CI_energy(1)
     enddo
    endif
    if(N_states.gt.1)then
     print*,'Variational + perturbative Energy difference'
     do i = 2, N_st
      print*,'Delta E = ',E_before(i)+ pt2(i) - (E_before(1) + pt2(1))
     enddo
    endif
    E_before = CI_energy
    call ezfio_set_ddci_selected_energy(CI_energy)
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
