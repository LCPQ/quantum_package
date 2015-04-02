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
  if (N_det > n_det_max_fci) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = n_det_max_fci
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

  do while (N_det < n_det_max_fci.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    call H_apply_FCI(pt2, norm_pert, H_pert_diag,  N_st)

    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det > n_det_max_fci) then
       psi_det = psi_det_sorted
       psi_coef = psi_coef_sorted
       N_det = n_det_max_fci
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

  ! Check that it is a CAS-SD
  logical :: in_cas
  integer :: exc_max
  exc_max = 0
  print *,  'CAS determinants'
  do i=1,N_det_generators
    do k=i,N_det_generators
      call get_excitation_degree(psi_det_generators(1,1,k),psi_det_generators(1,1,i),degree,N_int)
      exc_max = max(exc_max,degree)
    enddo
    call debug_det(psi_det_generators(1,1,i),N_int)
    print *,  ''
  enddo
  print *,  'Max excitation degree in the CAS :', exc_max
  do i=1,N_det
    in_cas = .False.
    do k=1,N_det_generators
      call get_excitation_degree(psi_det_generators(1,1,k),psi_det(1,1,i),degree,N_int)
      if (degree == 0) then
        in_cas = .True.
        exit
      endif
    enddo
    if (.not.in_cas) then
      do k=i,N_det
        call get_excitation_degree(psi_det(1,1,k),psi_det(1,1,i),degree,N_int)
        if (degree > exc_max+2) then
          print *,  'Error : This is not a CAS-SD : '
          print *,  'CAS determinant:'
          call debug_det(psi_det(1,1,i),N_int)
          print *,  'Excited determinant:', degree
          call debug_det(psi_det(1,1,k),N_int)
          stop
        endif
      enddo
    endif
  enddo
end
