program mrcc_stoch
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
  
  double precision, allocatable  :: mrcc(:)
  integer                        :: degree
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in

  double precision               :: E_CI_before, relative_error

  allocate (mrcc(N_states))
  mrcc = 0.d0
  !call random_seed()
  E_CI_before = mrcc_E0_denominator(1) + nuclear_repulsion
  threshold_selectors = 1.d0
  threshold_generators = 1d0 
  relative_error = 5.d-2
  call ZMQ_mrcc(E_CI_before, mrcc, relative_error)
  !print *,  'Final step'
  !print *,  'N_det    = ', N_det
  print *,  'mrcc      = ', mrcc
  !print *,  'E        = ', E_CI_before
  !print *,  'E+mrcc    = ', E_CI_before+mrcc
  !print *,  '-----'
  !call ezfio_set_full_ci_zmq_energy_mrcc(E_CI_before+mrcc(1))
end


