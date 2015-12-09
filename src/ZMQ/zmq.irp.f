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
   print *,  'This run should be started with the qp_run command'
   stop -1
 endif

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


function new_zmq_to_qp_run_socket() 
 implicit none
 BEGIN_DOC
 ! Socket on which the qp_run process replies
 END_DOC
 integer                        :: rc
 character*(8), external        :: zmq_port
 integer(ZMQ_PTR)               :: new_zmq_to_qp_run_socket

 new_zmq_to_qp_run_socket = f77_zmq_socket(zmq_context, ZMQ_REQ)
 rc = f77_zmq_connect(new_zmq_to_qp_run_socket, trim(qp_run_address)//':'//trim(zmq_port(0)))
 if (rc /= 0) then
   stop 'Unable to connect new_zmq_to_qp_run_socket'
 endif
 integer                        :: i
 i=4
 rc = f77_zmq_setsockopt(new_zmq_to_qp_run_socket, ZMQ_SNDTIMEO, 120000, i)
 if (rc /= 0) then
   stop 'Unable to set send timout in new_zmq_to_qp_run_socket'
 endif
 rc = f77_zmq_setsockopt(new_zmq_to_qp_run_socket, ZMQ_RCVTIMEO, 120000, i)
 if (rc /= 0) then
   stop 'Unable to set recv timout in new_zmq_to_qp_run_socket'
 endif
end


 BEGIN_PROVIDER [ integer(ZMQ_PTR), zmq_socket_pull ]
&BEGIN_PROVIDER [ character*(128), zmq_socket_pull_tcp_address ]
&BEGIN_PROVIDER [ character*(128), zmq_socket_push_tcp_address ]
&BEGIN_PROVIDER [ character*(128), zmq_socket_pull_inproc_address ]
  implicit none
  BEGIN_DOC
  ! Socket which pulls the results (2)
  END_DOC
  integer                        :: rc
  character*(8), external        :: zmq_port
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket

  zmq_socket_pull_tcp_address    = 'tcp://*:'//zmq_port(1)
  zmq_socket_push_tcp_address    = trim(qp_run_address)//':'//zmq_port(1)
  zmq_socket_pull_inproc_address = 'inproc://'//zmq_port(1)

!  zmq_socket_pull = f77_zmq_socket(zmq_context, ZMQ_PULL)
  zmq_socket_pull = f77_zmq_socket(zmq_context, ZMQ_REP )
  rc = f77_zmq_bind(zmq_socket_pull, zmq_socket_pull_tcp_address)
  rc = f77_zmq_bind(zmq_socket_pull, zmq_socket_pull_inproc_address)
  if (rc /= 0) then
    stop 'Unable to bind zmq_socket_pull (tcp)'
  endif

END_PROVIDER


 BEGIN_PROVIDER [ integer(ZMQ_PTR), zmq_thread, (0:nproc) ]
&BEGIN_PROVIDER [ character*(128), zmq_state ]
  implicit none
  BEGIN_DOC
! Threads executing work through the ZeroMQ interface
  END_DOC
  zmq_thread = 0_ZMQ_PTR
  zmq_state = 'No_state'
END_PROVIDER

subroutine new_parallel_job(zmq_to_qp_run_socket,name)
  implicit none
  BEGIN_DOC
! Start a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'
  END_DOC
  character*(*), intent(in)      :: name

  character*(512)                :: message
  integer                        :: rc
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR), intent(out)  :: zmq_to_qp_run_socket

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  message = 'new_job '//name//' '//zmq_socket_push_tcp_address//' '//zmq_socket_pull_inproc_address
  rc = f77_zmq_send(zmq_to_qp_run_socket,message,len(trim(message)),0)
  rc = f77_zmq_recv(zmq_to_qp_run_socket,message,510,0)
  message = trim(message(1:rc))
  if (message(1:2) /= 'ok') then
    print *,  'Unable to start parallel job : '//name
    stop 1
  endif

  zmq_state = name
  SOFT_TOUCH zmq_state zmq_thread

end

subroutine new_parallel_threads(slave,collector)
  implicit none
  BEGIN_DOC
! Start a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'
  END_DOC
  external                       :: slave, collector
  integer                        :: i,rc


  rc = pthread_create( zmq_thread(0), collector)
  do i=1,nproc
    rc = pthread_create( zmq_thread(i), slave )
  enddo
  SOFT_TOUCH zmq_thread zmq_state

end

subroutine connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  implicit none
  BEGIN_DOC
! Connect to the task server and obtain the worker ID
  END_DOC
  integer, intent(out)           :: worker_id
  integer, intent(in)            :: thread
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  
  character*(512)                :: message
  character*(128)                :: reply, state, address
  integer                        :: rc

  if (thread == 1) then
    rc = f77_zmq_send(zmq_to_qp_run_socket, "connect inproc", 14, 0)
  else
    rc = f77_zmq_send(zmq_to_qp_run_socket, "connect tcp", 11, 0)
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket, message, 510, 0)
  message = trim(message(1:rc))
  read(message,*) reply, state, worker_id, address
  if ( (trim(reply) /= 'connect_reply') .and. &
        (trim(state) /= trim(zmq_state)) ) then
      print *,  'Reply: ', trim(reply)
      print *,  'State: ', trim(state), '/', trim(zmq_state)
      print *,  'Address: ', trim(address)
      stop -1
  endif

end

subroutine disconnect_from_taskserver(zmq_to_qp_run_socket,worker_id,finished)
  implicit none
  BEGIN_DOC
! Disconnect from the task server
  END_DOC
  integer, intent(in)            :: worker_id
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(out)           :: finished

  integer                        :: rc
  character*(64)                 :: message, reply, state
  write(message,*) 'disconnect '//trim(zmq_state), worker_id

  rc = f77_zmq_send(zmq_to_qp_run_socket, trim(message), len(trim(message)), 0)

  rc = f77_zmq_recv(zmq_to_qp_run_socket, message, 510, 0)
  message = trim(message(1:rc))

  read(message,*) reply, state, finished
  if ( (trim(reply) /= 'disconnect_reply').or.                       &
        (trim(state) /= zmq_state) ) then
    print *,  'Unable to disconnect'
    print *,  trim(message)
    stop -1
  endif

end

subroutine add_task_to_taskserver(zmq_to_qp_run_socket,task)
  implicit none
  BEGIN_DOC
! Get a task from the task server
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  character*(*), intent(in)      :: task
  
  integer                       :: rc
  character*(512)               :: message
  write(message,*) 'add_task '//trim(zmq_state)//' '//trim(task)

  rc = f77_zmq_send(zmq_to_qp_run_socket, trim(message), len(trim(message)), 0)

  rc = f77_zmq_recv(zmq_to_qp_run_socket, message, 510, 0)
  message = trim(message(1:rc))
  if (trim(message) /= 'ok') then
    print *,  trim(task)
    print *,  'Unable to add the next task'
    stop -1
  endif

end

subroutine task_done_to_taskserver(zmq_to_qp_run_socket,worker_id, task_id)
  implicit none
  BEGIN_DOC
! Get a task from the task server
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id, task_id
  
  integer                        :: rc
  character*(512)                :: message
  write(message,*) 'task_done '//trim(zmq_state), worker_id, task_id

  rc = f77_zmq_send(zmq_to_qp_run_socket, trim(message), len(trim(message)), 0)

  rc = f77_zmq_recv(zmq_to_qp_run_socket, message, 510, 0)
  message = trim(message(1:rc))
  if (trim(message) /= 'ok') then
    print *,  'Unable to send task_done message'
    stop -1
  endif

end

subroutine get_task_from_taskserver(zmq_to_qp_run_socket,worker_id,task_id,task)
  implicit none
  BEGIN_DOC
! Get a task from the task server
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer, intent(in)            :: worker_id
  integer, intent(out)           :: task_id
  character*(512), intent(out)   :: task

  character*(512)                :: message
  character*(64)                 :: reply
  integer                        :: rc

  write(message,*) 'get_task '//trim(zmq_state), worker_id

  rc = f77_zmq_send(zmq_to_qp_run_socket, trim(message), len(trim(message)), 0)

  rc = f77_zmq_recv(zmq_to_qp_run_socket, message, 510, 0)
  message = trim(message(1:rc))
  read(message,*) reply
  if (trim(reply) == 'get_task_reply') then
    read(message,*) reply, task_id
    rc = 15
    do while (message(rc:rc) == ' ')
      rc += 1
    enddo
    do while (message(rc:rc) /= ' ')
      rc += 1
    enddo
    rc += 1
    task = message(rc:)
  else if (trim(reply) == 'terminate') then
    task_id = 0
    task = 'terminate'
  else
    print *,  'Unable to get the next task'
    print *,  trim(message)
    stop -1
  endif

end


subroutine end_parallel_job(zmq_to_qp_run_socket,name)
  implicit none
  BEGIN_DOC
! End a new parallel job with name 'name'. The slave tasks execute subroutine 'slave'
  END_DOC
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  character*(*), intent(in)      :: name

  character*(512)                :: message
  integer                        :: i,rc

  if (name /= zmq_state) then
    stop 'Wrong end of job'
  endif

  ! Wait for Slaves
  do i=1,nproc
    rc = pthread_join( zmq_thread(i) )
    if (rc /= 0) then
      print *,  'Unable to join thread : ', i
      stop -1
    endif
    zmq_thread(i) = 0
    print *,  'joined ', i
  enddo
  ! Wait for collector
  rc = pthread_join( zmq_thread(0) )
  zmq_thread(0) = 0
    print *,  'joined ', 0
  zmq_state = 'No_state'
  character*(8), external        :: zmq_port
  rc = f77_zmq_disconnect(zmq_to_qp_run_socket, trim(qp_run_address)//':'//trim(zmq_port(0)))
  rc = f77_zmq_close(zmq_to_qp_run_socket)


  SOFT_TOUCH zmq_thread zmq_state

end

