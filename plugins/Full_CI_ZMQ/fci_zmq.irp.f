program fci_zmq
  implicit none
  integer                        :: i,j,k
  logical, external              :: detEq
  
  double precision, allocatable  :: pt2(:)
  integer                        :: degree
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in
  
  allocate (pt2(N_states))
  
  pt2 = 1.d0
  threshold_davidson_in = threshold_davidson
  threshold_davidson = threshold_davidson_in * 100.d0
  SOFT_TOUCH threshold_davidson
  
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
    do k=1,N_states
      print*,'State ',k
      print *,  'PT2      = ', pt2(k)
      print *,  'E        = ', CI_energy(k)
      print *,  'E+PT2    = ', CI_energy(k) + pt2(k)
      print *,  '-----'
    enddo
  endif
  double precision               :: E_CI_before(N_states)
  
  
  print*,'Beginning the selection ...'
  E_CI_before(1:N_states) = CI_energy(1:N_states)
  n_det_before = 0
  
  do while ( (N_det < N_det_max) .and. (maxval(abs(pt2(1:N_states))) > pt2_max) )
    
    print *,  'N_det          = ', N_det
    print *,  'N_states       = ', N_states
    do k=1, N_states
      print*,'State ',k
      print *,  'PT2            = ', pt2(k)
      print *,  'E              = ', CI_energy(k)
      print *,  'E(before)+PT2  = ', E_CI_before(k)+pt2(k)
    enddo
    print *,  '-----'
    if(N_states.gt.1)then
      print*,'Variational Energy difference'
      do i = 2, N_states
        print*,'Delta E = ',CI_energy(i) - CI_energy(1)
      enddo
    endif
    if(N_states.gt.1)then
      print*,'Variational + perturbative Energy difference'
      do i = 2, N_states
        print*,'Delta E = ',E_CI_before(i)+ pt2(i) - (E_CI_before(1) + pt2(1))
      enddo
    endif
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))

    n_det_before = N_det
    to_select = 2*N_det
    to_select = max(64-to_select, to_select)
    to_select = min(to_select, N_det_max-n_det_before)
    call ZMQ_selection(to_select, pt2)
    
    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det == N_det_max) then
      threshold_davidson = threshold_davidson_in
      SOFT_TOUCH threshold_davidson
    endif
    call diagonalize_CI
    call save_wavefunction
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  enddo

  if (N_det < N_det_max) then
      threshold_davidson = threshold_davidson_in
      SOFT_TOUCH threshold_davidson
      call diagonalize_CI
      call save_wavefunction
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  endif

  if(do_pt2_end)then
    print*,'Last iteration only to compute the PT2'
    threshold_selectors = max(threshold_selectors,threshold_selectors_pt2)
    threshold_generators = max(threshold_generators,threshold_generators_pt2)
    TOUCH threshold_selectors threshold_generators
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ZMQ_selection(0, pt2)
    print *,  'Final step'
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    do k=1,N_states
      print *, 'State', k
      print *,  'PT2      = ', pt2
      print *,  'E        = ', E_CI_before
      print *,  'E+PT2    = ', E_CI_before+pt2
      print *,  '-----'
    enddo
    call ezfio_set_full_ci_zmq_energy_pt2(E_CI_before(1)+pt2(1))
  endif
  call save_wavefunction
  call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  call ezfio_set_full_ci_zmq_energy_pt2(E_CI_before(1)+pt2(1))
end
