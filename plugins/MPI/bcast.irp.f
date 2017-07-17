subroutine mpi_bcast_psi()
  use f77_zmq
  implicit none
  BEGIN_DOC
! Broadcast the wave function coming from the qp_run scheduler
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
  IRP_ENDIF

end

