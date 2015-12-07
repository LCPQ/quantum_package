subroutine ao_bielec_integrals_in_map_slave_tcp
  implicit none
  BEGIN_DOC
! Computes a buffer of integrals
  END_DOC
  call ao_bielec_integrals_in_map_slave(0)
end

subroutine ao_bielec_integrals_in_map_slave_inproc
  implicit none
  BEGIN_DOC
! Computes a buffer of integrals
  END_DOC
  call ao_bielec_integrals_in_map_slave(1)
end

subroutine ao_bielec_integrals_in_map_slave(thread)
  use map_module
  use f77_zmq
  implicit none
  BEGIN_DOC
! Computes a buffer of integrals
  END_DOC

  integer, intent(in)            :: thread

  integer                        :: j,l,n_integrals
  integer                        :: rc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)
  
  integer                        :: worker_id, task_id
  character*(512)                :: task

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  
  integer(ZMQ_PTR)               :: zmq_socket_push
  zmq_socket_push = f77_zmq_socket(zmq_context, ZMQ_PUSH)
  if (thread == 1) then
    rc = f77_zmq_connect(zmq_socket_push, trim(zmq_socket_pull_inproc_address))
  else
    rc = f77_zmq_connect(zmq_socket_push, trim(zmq_socket_push_tcp_address))
  endif
  if (rc /= 0) then
    stop 'Unable to connect zmq_socket_push_tcp'
  endif

  allocate ( buffer_i(ao_num*ao_num), buffer_value(ao_num*ao_num) )

  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)

  do 
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task)
    if (task_id == 0) then
      exit
    endif
    read(task,*) j, l
    call compute_ao_integrals_jl(j,l,n_integrals,buffer_i,buffer_value) 
    rc = f77_zmq_send( zmq_socket_push, n_integrals, 4, ZMQ_SNDMORE)
    rc = f77_zmq_send( zmq_socket_push, buffer_i, key_kind*n_integrals, ZMQ_SNDMORE)
    rc = f77_zmq_send( zmq_socket_push, buffer_value, integral_kind*n_integrals, 0)
    call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
  enddo

  deallocate( buffer_i, buffer_value )

  integer :: finished
  call disconnect_from_taskserver(zmq_to_qp_run_socket,worker_id,finished)

  if (finished /= 0) then
    rc = f77_zmq_send( zmq_socket_push, -1, 4, 0)
  endif

  rc = f77_zmq_disconnect(zmq_socket_push,trim(zmq_socket_push_tcp_address))
  rc = f77_zmq_close(zmq_socket_push)

end


subroutine ao_bielec_integrals_in_map_collector
  use map_module
  use f77_zmq
  implicit none
  BEGIN_DOC
! Collects results from the AO integral calculation
  END_DOC

  integer                        :: j,l,n_integrals
  integer                        :: rc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)

  allocate ( buffer_i(ao_num*ao_num), buffer_value(ao_num*ao_num) )

  n_integrals = 0
  do while (n_integrals >= 0)
    
    rc = f77_zmq_recv( zmq_socket_pull, n_integrals, 4, 0)
    if (n_integrals >= 0) then
      rc = f77_zmq_recv( zmq_socket_pull, buffer_i, key_kind*n_integrals, 0)
      rc = f77_zmq_recv( zmq_socket_pull, buffer_value, integral_kind*n_integrals, 0)
      call insert_into_ao_integrals_map(n_integrals,buffer_i,buffer_value)
    endif

  enddo

  deallocate( buffer_i, buffer_value )

end

