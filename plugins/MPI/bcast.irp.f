subroutine mpi_bcast_psi()
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put the wave function on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: ierr
  character*(256)                :: msg

  IRP_IF MPI
    call MPI_BCast(N_states,     1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call MPI_BCast(N_det,        1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    call MPI_BCast(psi_det_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  
    TOUCH psi_det_size N_det N_states

    call MPI_BCast(psi_det,         N_det, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)
    call MPI_BCast(psi_coef, psi_det_size, MPI_DOUBLE_PRECISION* N_states, 0, MPI_COMM_WORLD, ierr)


  rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_coef,int(psi_det_size*N_states*8_8,8),0)
  if (rc8 /= psi_det_size*N_states*8_8) then
    print *, '77_zmq_recv8(zmq_to_qp_run_socket,psi_coef,psi_det_size*N_states*8,ZMQ_SNDMORE)'
    stop 'error'
  endif
  TOUCH psi_det psi_coef

  rc = f77_zmq_recv(zmq_to_qp_run_socket,energy,size_energy*8,0)
  if (rc /= size_energy*8) then
    print *, '77_zmq_recv(zmq_to_qp_run_socket,energy,size_energy*8,0)'
    stop 'error'
  endif

  if (N_det_generators_read > 0) then
    N_det_generators = N_det_generators_read
    TOUCH N_det_generators
  endif
  if (N_det_selectors_read > 0) then
    N_det_selectors  = N_det_selectors_read
    TOUCH N_det_selectors
  endif

end

