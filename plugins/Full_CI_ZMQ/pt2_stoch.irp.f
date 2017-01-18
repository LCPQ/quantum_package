program pt2_stoch
  implicit none
  initialize_pt2_E0_denominator = .False.
  read_wf = .True.
  SOFT_TOUCH initialize_pt2_E0_denominator read_wf
  PROVIDE mo_bielec_integrals_in_map
  call run
end

subroutine run
  implicit none
  integer                        :: i,j,k
  logical, external              :: detEq
  
  double precision, allocatable  :: pt2(:)
  integer                        :: degree
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in

  double precision               :: E_CI_before(N_states), relative_error

  if (.true.) then
    call ezfio_get_full_ci_zmq_energy(E_CI_before(1))
    pt2_e0_denominator(:) = E_CI_before(1) - nuclear_repulsion
    SOFT_TOUCH pt2_e0_denominator read_wf
  endif
  allocate (pt2(N_states))
  
  threshold_selectors = 1.d0
  threshold_generators = 1d0 
  relative_error = 1.d-3
  call ZMQ_pt2(pt2, relative_error)
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
end


