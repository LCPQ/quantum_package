use f77_zmq


BEGIN_PROVIDER [ integer(ZMQ_PTR), zmq_context ]
 implicit none
 BEGIN_DOC
 ! Context for the ZeroMQ library
 END_DOC
 zmq_context = f77_zmq_ctx_new ()
END_PROVIDER


 BEGIN_PROVIDER [ character*(128), qp_run_address ]
&BEGIN_PROVIDER [ integer, zmq_port_start ]
 implicit none
 BEGIN_DOC
 ! Address of the qp_run socket
 ! Example : tcp://130.120.229.139:12345
 END_DOC
 character*(128) :: buffer
 call getenv('QP_RUN_ADDRESS',buffer)
 if (trim(buffer) == '') then
   stop 'QP_RUN_ADDRESS environment variable not defined'
 endif

 print *,  trim(buffer)
 integer :: i
 do i=len(buffer),1,-1
   if ( buffer(i:i) == ':') then
     qp_run_address = trim(buffer(1:i-1))
     read(buffer(i+1:), *) zmq_port_start
     exit
   endif
 enddo
END_PROVIDER

   
function zmq_port(ishift)
  implicit none
  integer, intent(in)            :: ishift
  character*(8)                  :: zmq_port
  write(zmq_port,'(I8)') zmq_port_start+ishift
  zmq_port = adjustl(trim(zmq_port))
end


BEGIN_PROVIDER [ integer(ZMQ_PTR), zmq_to_qp_run_socket ]
 implicit none
 BEGIN_DOC
 ! Socket on which the qp_run process replies
 END_DOC
 integer                        :: rc
 zmq_to_qp_run_socket = f77_zmq_socket(zmq_context, ZMQ_REQ)
 rc = f77_zmq_connect(zmq_to_qp_run_socket, trim(qp_run_address))
 if (rc /= 0) then
   stop 'Unable to connect zmq_to_qp_run_socket'
 endif
 integer                        :: i
 i=4
 rc = f77_zmq_setsockopt(zmq_to_qp_run_socket, ZMQ_SNDTIMEO, 120000, i)
 if (rc /= 0) then
   stop 'Unable to set send timout in zmq_to_qp_run_socket'
 endif
 rc = f77_zmq_setsockopt(zmq_to_qp_run_socket, ZMQ_RCVTIMEO, 120000, i)
 if (rc /= 0) then
   stop 'Unable to set recv timout in zmq_to_qp_run_socket'
 endif
END_PROVIDER

BEGIN_PROVIDER [ integer(ZMQ_PTR), zmq_socket_push ]
  implicit none
  BEGIN_DOC
  ! Socket on which to push the results (1)
  END_DOC
  integer                        :: rc
  character*(64)                 :: address
  character*(8), external        :: zmq_port
  zmq_socket_push = f77_zmq_socket(zmq_context, ZMQ_PUSH)
  address = trim(qp_run_address)//':'//zmq_port(1)
  rc = f77_zmq_connect(zmq_socket_push, trim(address))
  if (rc /= 0) then
    stop 'Unable to connect zmq_socket_push'
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ integer(ZMQ_PTR), zmq_socket_pull ]
  implicit none
  BEGIN_DOC
  ! Socket which pulls the results (2)
  END_DOC
  integer                        :: rc
  character*(64)                 :: address
  character*(8), external        :: zmq_port
  zmq_socket_pull = f77_zmq_socket(zmq_context, ZMQ_PULL)
  address = 'tcp://*:'//zmq_port(2)
  rc = f77_zmq_bind(zmq_socket_pull, trim(address))
  if (rc /= 0) then
    stop 'Unable to connect zmq_socket_pull'
  endif
  
END_PROVIDER



