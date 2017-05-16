program davidson_slave
  use f77_zmq
  implicit none


  integer(ZMQ_PTR), external :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR) :: zmq_to_qp_run_socket
  double precision :: energy(N_states_diag)
  character*(64) :: state
  
  call provide_everything
  call switch_qp_run_to_master
  call omp_set_nested(.True.)
  
  zmq_context = f77_zmq_ctx_new ()
  zmq_state = 'davidson'
  state = 'Waiting'

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  do
    call wait_for_state(zmq_state,state)
    if(trim(state) /= "davidson") exit
    integer :: rc, i
    print *,  'Davidson slave running'
    call davidson_slave_tcp(i)
  end do
end

subroutine provide_everything
  PROVIDE mo_bielec_integrals_in_map psi_det_sorted_bit N_states_diag zmq_context ref_bitmask_energy
end subroutine

