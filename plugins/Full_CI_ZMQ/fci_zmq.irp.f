program Full_CI_ZMQ
  use f77_zmq
  implicit none
  BEGIN_DOC
! Massively parallel Full-CI
  END_DOC

  integer :: i,ithread

  integer(ZMQ_PTR) :: zmq_socket_push
  integer(ZMQ_PTR) :: new_zmq_push_socket
  zmq_context = f77_zmq_ctx_new ()
    

  PROVIDE nproc
  !$OMP PARALLEL PRIVATE(i,ithread,zmq_socket_push) num_threads(nproc+1)
  ithread = omp_get_thread_num()
  if (ithread == 0) then
    call receive_selected_determinants()
  else
    zmq_socket_push = new_zmq_push_socket()
    do i=ithread,N_det_generators,nproc
      print *,  i , N_det_generators
      !$OMP TASK DEFAULT(SHARED)
      call select_connected(i, 1.d-6, ci_electronic_energy,zmq_socket_push)
      !$OMP END TASK
    enddo
    !$OMP TASKWAIT
    if (ithread == 1) then
      integer :: rc
      rc = f77_zmq_send(zmq_socket_push,0,1,0)
      if (rc /= 1) then
        stop 'Error sending termination signal'
      endif
    endif
    call end_zmq_push_socket(zmq_socket_push)

  endif
  !$OMP END PARALLEL
      

end
