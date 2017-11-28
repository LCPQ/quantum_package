subroutine zmq_put_psi(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put the wave function on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  character*(256)                :: msg

  call zmq_put_N_states(zmq_to_qp_run_socket, worker_id)
  call zmq_put_N_det(zmq_to_qp_run_socket, worker_id)
  call zmq_put_psi_det_size(zmq_to_qp_run_socket, worker_id)
  call zmq_put_psi_det(zmq_to_qp_run_socket, worker_id)
  call zmq_put_psi_coef(zmq_to_qp_run_socket, worker_id)

end



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

N_states ;;
N_det ;;
psi_det_size ;;

END_TEMPLATE

subroutine zmq_put_psi_det(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put psi_det on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  write(msg,'(A8,1X,I8,1X,A230)') 'put_data', worker_id, 'psi_det'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error sending psi_det'
    stop 'error'
  endif

  rc8 = f77_zmq_send8(zmq_to_qp_run_socket,psi_det,int(N_int*2_8*N_det*bit_kind,8),0)
  if (rc8 /= N_int*2_8*N_det*bit_kind) then
    print *,  irp_here, ': Error sending psi_det'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in put_data_reply'
    stop 'error'
  endif
end

subroutine zmq_put_psi_coef(zmq_to_qp_run_socket,worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Put psi_coef on the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  write(msg,'(A8,1X,I8,1X,A230)') 'put_data', worker_id, 'psi_coef'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),ZMQ_SNDMORE)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error sending psi_coef'
    stop 'error'
  endif

  rc8 = f77_zmq_send8(zmq_to_qp_run_socket,psi_coef,int(psi_det_size*N_states*8_8,8),0)
  if (rc8 /= psi_det_size*N_states*8_8) then
    print *,  irp_here, ': Error sending psi_coef'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:rc) /= 'put_data_reply ok') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in put_data_reply'
    stop 'error'
  endif
end

!---------------------------------------------------------------------------


subroutine zmq_get_psi(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get the wave function from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id

  call zmq_get_N_states(zmq_to_qp_run_socket, worker_id)
  call zmq_get_N_det(zmq_to_qp_run_socket, worker_id)
  call zmq_get_psi_det_size(zmq_to_qp_run_socket, worker_id)
  TOUCH psi_det_size N_det N_states

  call zmq_get_psi_det(zmq_to_qp_run_socket, worker_id)
  call zmq_get_psi_coef(zmq_to_qp_run_socket, worker_id)
  SOFT_TOUCH psi_det psi_coef

end


subroutine zmq_get_psi_det(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get psi_det from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg

  
  write(msg,'(A8,1X,I8,1X,A230)') 'get_data', worker_id, 'psi_det' 
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error getting psi_det'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:14) /= 'get_data_reply') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in get_data_reply'
    stop 'error'
  endif

  rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_det,int(N_int*2_8*N_det*bit_kind,8),0)
  if (rc8 /= N_int*2_8*N_det*bit_kind) then
    print *,  irp_here, ': Error getting psi_det', rc8, N_int*2_8*N_det*bit_kind
    stop 'error'
  endif
  IRP_IF MPI
    call broadcast_chunks_bit_kind(psi_det,N_det*N_int*2)
  IRP_ENDIF

end

subroutine zmq_get_psi_coef(zmq_to_qp_run_socket, worker_id)
  use f77_zmq
  implicit none
  BEGIN_DOC
! Get psi_coef from the qp_run scheduler
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer                        :: rc
  integer*8                      :: rc8
  character*(256)                :: msg


  write(msg,'(A8,1X,I8,1X,A230)') 'get_data', worker_id, 'psi_coef'
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
  if (rc /= len(trim(msg))) then
    print *,  irp_here, ': Error getting psi_coef'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:14) /= 'get_data_reply') then
    print *,  rc, trim(msg)
    print *,  irp_here, ': Error in get_data_reply'
    stop 'error'
  endif

  rc8 = f77_zmq_recv8(zmq_to_qp_run_socket,psi_coef,int(psi_det_size*N_states*8_8,8),0)
  if (rc8 /= psi_det_size*N_states*8_8) then
    print *, irp_here, ': Error getting psi_coef'
    stop 'error'
  endif

  IRP_IF MPI
    call broadcast_chunks_double(psi_coef,N_states*N_det)
  IRP_ENDIF

end


