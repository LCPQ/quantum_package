BEGIN_TEMPLATE 

subroutine zmq_put_$X(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put $X on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  character*(256)                :: msg

  write(msg,'(A8,1X,I8,1X,A230)') 'put_data', worker_id, '$X'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error sending $X'
    stop 'error'
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,$X,4,0)
  if (rc /= 4) then
    print *,  irp_here, ': Error sending $X'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in put_data_reply'
    stop 'error'
  endif

end

subroutine zmq_get_$X(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get $X from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  character*(256)                :: msg

  write(msg,'(A8,1X,I8,1X,A230)') 'get_data', worker_id, '$X'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error getting $X'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:14) /= 'get_data_reply') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in get_data_reply'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,$X,4,0)
  if (rc /= 4) then
    print *,  rc
    print *,  irp_here, ': Error getting $X'
    stop 'error'
  endif
end

SUBST [ X ]

N_det_generators ;;
N_det_selectors ;;

END_TEMPLATE

