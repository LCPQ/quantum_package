program fci_zmq
  implicit none
  integer                        :: i,j,k
  double precision, allocatable  :: pt2(:)
  integer                        :: degree
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in
  
  allocate (pt2(N_states))

  double precision               :: hf_energy_ref
  logical                        :: has
  double precision :: relative_error, absolute_error
  relative_error=PT2_relative_error
  absolute_error=PT2_absolute_error

  pt2 = -huge(1.d0)
  threshold_davidson_in = threshold_davidson
  threshold_davidson = threshold_davidson_in * 100.d0
  SOFT_TOUCH threshold_davidson

  call diagonalize_CI
  call save_wavefunction
  
  call ezfio_has_hartree_fock_energy(has)
  if (has) then
    call ezfio_get_hartree_fock_energy(hf_energy_ref)
  else
    hf_energy_ref = ref_bitmask_energy
  endif

  if (N_det > N_det_max) then
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
    call dump_fci_iterations_value(N_det,CI_energy(1),pt2(1)) ! This call automatically appends data
  endif
  
  
  print*,'Beginning the selection ...'
  n_det_before = 0

  character*(8) :: pt2_string
  double precision :: correlation_energy_ratio
  double precision :: threshold_selectors_save, threshold_generators_save
  threshold_selectors_save  = threshold_selectors
  threshold_generators_save = threshold_generators
  double precision :: error

  correlation_energy_ratio = 0.d0

  if (.True.) then ! Avoid pre-calculation of CI_energy
    do while (                                                         &
          (N_det < N_det_max) .and.                                    &
          (maxval(abs(pt2(1:N_states))) > pt2_max) .and.               &
          (correlation_energy_ratio <= correlation_energy_ratio_max)    &
          )


      if (do_pt2) then
        pt2_string = '        '
        pt2 = 0.d0
        if (N_states == 1) then
          threshold_selectors = 1.d0
          threshold_generators = 1d0 
          SOFT_TOUCH threshold_selectors threshold_generators
          call ZMQ_pt2(CI_energy, pt2,relative_error,absolute_error,error) ! Stochastic PT2
          threshold_selectors = threshold_selectors_save
          threshold_generators = threshold_generators_save
          SOFT_TOUCH threshold_selectors threshold_generators
        else
          threshold_selectors = max(threshold_selectors,threshold_selectors_pt2)
          threshold_generators = max(threshold_generators,threshold_generators_pt2)
          SOFT_TOUCH threshold_selectors threshold_generators
          call ZMQ_selection(0, pt2)      ! Deterministic PT2
        endif
      else
        pt2_string = '(approx)'
      endif


      correlation_energy_ratio = (CI_energy(1) - hf_energy_ref)  /     &
                      (CI_energy(1) + pt2(1) - hf_energy_ref)
      correlation_energy_ratio = min(1.d0,correlation_energy_ratio)


      print *,  'N_det             = ', N_det
      print *,  'N_states          = ', N_states
      print*,   'correlation_ratio = ', correlation_energy_ratio

      do k=1, N_states
        print*,'State ',k
        print *,  'PT2             = ', pt2(k)
        print *,  'E               = ', CI_energy(k)
        if (N_states==1) then
          print *,  'E+PT2'//pt2_string//'   = ', CI_energy(k)+pt2(k), ' +/- ', error
        else
          print *,  'E+PT2'//pt2_string//'   = ', CI_energy(k)+pt2(k)
        endif
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
          print*,'Delta E = ',CI_energy(i)+ pt2(i) - (CI_energy(1) + pt2(1))
        enddo
      endif

      n_det_before = N_det
      to_select = N_det
      to_select = max(N_det, to_select)
      to_select = min(to_select, N_det_max-n_det_before)
      call ZMQ_selection(to_select, pt2)
      
      PROVIDE  psi_coef
      PROVIDE  psi_det
      PROVIDE  psi_det_sorted

      if (N_det >= N_det_max) then
        threshold_davidson = threshold_davidson_in
      end if
      call diagonalize_CI
      call save_wavefunction
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
      call ezfio_set_full_ci_zmq_energy_pt2(CI_energy(1)+pt2(1))
      call dump_fci_iterations_value(N_det,CI_energy(1),pt2(1)) ! This call automatically appends data
    enddo
  endif

  if (N_det < N_det_max) then
      threshold_davidson = threshold_davidson_in
      call diagonalize_CI
      call save_wavefunction
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
      call dump_fci_iterations_value(N_det,CI_energy(1),pt2(1)) ! This call automatically appends data
  endif

  if (do_pt2) then
    pt2 = 0.d0
    if (N_states == 1) then
      threshold_selectors = 1.d0
      threshold_generators = 1d0 
      SOFT_TOUCH threshold_selectors threshold_generators
      call ZMQ_pt2(CI_energy, pt2,relative_error,absolute_error,error) ! Stochastic PT2
      threshold_selectors = threshold_selectors_save
      threshold_generators = threshold_generators_save
      SOFT_TOUCH threshold_selectors threshold_generators
    else
      threshold_selectors = max(threshold_selectors,threshold_selectors_pt2)
      threshold_generators = max(threshold_generators,threshold_generators_pt2)
      SOFT_TOUCH threshold_selectors threshold_generators
      call ZMQ_selection(0, pt2)      ! Deterministic PT2
    endif
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))
    call ezfio_set_full_ci_zmq_energy_pt2(CI_energy(1)+pt2(1))
    call dump_fci_iterations_value(N_det,CI_energy(1),pt2(1)) ! This call automatically appends data
  endif
  print *,  'N_det             = ', N_det
  print *,  'N_states          = ', N_states
  print*,   'correlation_ratio = ', correlation_energy_ratio

  do k=1, N_states
    print*,'State ',k
    print *,  'PT2             = ', pt2(k)
    print *,  'E               = ', CI_energy(k)
    if (N_states==1) then
      print *,  'E+PT2'//pt2_string//'   = ', CI_energy(k)+pt2(k), ' +/- ', error
    else
      print *,  'E+PT2'//pt2_string//'   = ', CI_energy(k)+pt2(k)
    endif
  enddo

  print *,  '-----'


end
