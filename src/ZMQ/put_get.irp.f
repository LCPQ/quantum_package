subroutine zmq_put_dvector(zmq_to_qp_run_socket, worker_id, name, x, size_x)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put the X vector on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  character*(*)                  :: name
  integer, intent(in)            :: size_x
  double precision, intent(out)  :: x(size_x)
  integer                        :: rc
  character*(256)                :: msg


  write(msg,'(A8,1X,I8,1X,A230)') 'put_data', worker_id, name
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error sending '//name
    stop 'error'
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,x,size_x*8,0)
  if (rc /= size_x*8) then
    print *,  irp_here, ': Error sending '//name
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in put_data_reply'
    stop 'error'
  endif

end


subroutine zmq_get_dvector(zmq_to_qp_run_socket, worker_id, name, x, size_x)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get psi_coef from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer, intent(in)            :: size_x
  character*(*), intent(in)      :: name
  double precision, intent(out)  :: x(size_x)
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  write(msg,'(A8,1X,I8,1X,A230)') 'get_data', worker_id, name
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error getting '//name
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:14) /= 'get_data_reply') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in get_data_reply'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,x,size_x*8,0)
  if (rc /= size_x*8) then
    print *, irp_here, ': Error getting '//name
    stop 'error'
  endif
end



