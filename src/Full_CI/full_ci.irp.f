program cisd
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  
  pt2 = 1.d0
  diag_algorithm = "Lapack"
  do while (maxval(abs(pt2(1:N_st))) > 1.d-3)
    call H_apply_FCI(pt2, norm_pert, H_pert_diag,  N_st)
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
    if (abort_all) then
      exit
    endif
  enddo
  print *,  'Final step'
  call remove_small_contributions
  call diagonalize_CI
  print *,  'N_det    = ', N_det
  print *,  'N_states = ', N_states
  print *,  'PT2      = ', pt2
  print *,  'E        = ', CI_energy
  print *,  'E+PT2    = ', CI_energy+pt2
  print *,  '-----'
  deallocate(pt2,norm_pert)
end
