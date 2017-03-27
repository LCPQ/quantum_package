program fci_zmq
  implicit none
  integer                        :: i,j,k
  logical, external              :: detEq
  
  double precision, allocatable  :: pt2(:)
  integer                        :: Nmin, Nmax
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in, ratio, E_ref, pt2_ratio
  
  allocate (pt2(N_states))
  
  pt2 = 1.d0
  threshold_davidson_in = threshold_davidson
  threshold_davidson = threshold_davidson_in * 100.d0
  SOFT_TOUCH threshold_davidson 
  
  double precision               :: E_CI_before(N_states)
  do while (dabs(pt2(1)) > pt2_max)
    print *,  'N_det          = ', N_det
    print *,  'N_states       = ', N_states
    do k=1, N_states
      print*,'State ',k
      print *,  'PT2            = ', pt2(k)
      print *,  'E              = ', CI_energy(k)
      print *,  'E(before)+PT2  = ', E_CI_before(k)+pt2(k)
    enddo
    print *,  '-----'
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))

    n_det_before = N_det
    to_select = N_det
    to_select = max(64-to_select, to_select)
    call ZMQ_selection(to_select, pt2)
    
    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    call diagonalize_CI
    call save_wavefunction
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  enddo

  threshold_selectors = max(threshold_selectors,threshold_selectors_pt2)
  threshold_generators = max(threshold_generators,threshold_generators_pt2)
  threshold_davidson = threshold_davidson_in
  TOUCH threshold_selectors threshold_generators threshold_davidson
  call diagonalize_CI
  call ZMQ_selection(0, pt2)

  E_ref = CI_energy(1) + pt2(1)
  pt2_ratio = (E_ref + pt2_max - HF_energy) / (E_ref - HF_energy)
  print *,  'Est FCI   = ', E_ref

  Nmax = N_det
  Nmin = N_det/8
  do while (Nmax-Nmin > 1)
    call diagonalize_CI
    ratio = (CI_energy(1) - HF_energy) / (E_ref - HF_energy)
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    TOUCH psi_coef psi_det
    if (ratio < pt2_ratio) then
        Nmin = N_det
        to_select = (Nmax-Nmin)/2
        call ZMQ_selection(to_select, pt2)
    else
        Nmax = N_det
        N_det = Nmin + (Nmax-Nmin)/2
    endif
    print *,  '-----'
    print *,  'Det min, Det max: ', Nmin, Nmax
    print *,  'Ratio           : ', ratio, '  ~  ', pt2_ratio
    print *,  'HF_energy = ', HF_energy
    print *,  'Est FCI   = ', E_ref
    print *,  'N_det     = ', N_det
    print *,  'E         = ', CI_energy(1)
    print *,  'PT2       = ', pt2(1)
  enddo
  call ZMQ_selection(0, pt2)
  print *,  '------'
  print *,  'E         = ', CI_energy(1)
  print *,  'PT2       = ', pt2(1)

  E_CI_before(1:N_states) = CI_energy(1:N_states)
  call save_wavefunction
  call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  call ezfio_set_full_ci_zmq_energy_pt2(E_CI_before(1)+pt2(1))
end




