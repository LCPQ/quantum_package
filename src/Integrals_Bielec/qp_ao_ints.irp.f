program qp_ao_ints
  implicit none
  BEGIN_DOC
! Increments a running calculation to compute AO integrals
  END_DOC

  ! Set the state of the ZMQ
  zmq_state = 'ao_integrals'

  ! Provide everything needed
  double precision :: integral, ao_bielec_integral
  integral = ao_bielec_integral(1,1,1,1)

  !$OMP PARALLEL DEFAULT(PRIVATE)
  call ao_bielec_integrals_in_map_slave_tcp
  !$OMP END PARALLEL

  print *,  'Done'
end

