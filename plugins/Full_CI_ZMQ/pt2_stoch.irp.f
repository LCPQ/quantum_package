program pt2_stoch
  implicit none
  read_wf = .True.
  SOFT_TOUCH read_wf
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

  double precision               :: E_CI_before, relative_error, absolute_error, eqt

  allocate (pt2(N_states))
  call diagonalize_CI()
  pt2 = 0.d0
  
  E_CI_before = pt2_E0_denominator(1) + nuclear_repulsion
  threshold_selectors = 1.d0
  threshold_generators = 1d0 
  relative_error = 1.d-9
  absolute_error = 1.d-9
  call ZMQ_pt2(E_CI_before, pt2, relative_error, absolute_error, eqt)
  print *,  'Final step'
  print *,  'N_det    = ', N_det
  print *,  'PT2      = ', pt2
  print *,  'E        = ', E_CI_before
  print *,  'E+PT2    = ', E_CI_before+pt2, ' +/- ', eqt
  print *,  '-----'
  call ezfio_set_full_ci_zmq_energy_pt2(E_CI_before+pt2(1))
end


