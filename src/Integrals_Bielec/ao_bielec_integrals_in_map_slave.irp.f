subroutine ao_bielec_integrals_in_map_slave
  use map_module
  use f77_zmq
  implicit none
  BEGIN_DOC
! Computes a buffer of integrals
  END_DOC

  integer                        :: j,l,n_integrals
  integer                        :: rc
  character*(8), external        :: zmq_port
  integer(ZMQ_PTR)               :: zmq_socket_req_inproc, zmq_socket_push_inproc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)

  allocate ( buffer_i(ao_num*ao_num), buffer_value(ao_num*ao_num) )

  ! Sockets
  zmq_socket_req_inproc = f77_zmq_socket(zmq_context, ZMQ_REQ)
  rc = f77_zmq_connect(zmq_socket_req_inproc, 'inproc://req_rep')
  if (rc /= 0) then
    stop 'Unable to connect zmq_socket_req_inproc'
  endif

  zmq_socket_push_inproc = f77_zmq_socket(zmq_context, ZMQ_PUSH)
  rc = f77_zmq_connect(zmq_socket_push_inproc, 'inproc://push_pull')
  if (rc /= 0) then
    stop 'Unable to connect zmq_socket_push_inproc'
  endif



  rc = f77_zmq_send( zmq_socket_req_inproc, 'get_ao_integrals', 16, 0)
  rc = f77_zmq_recv( zmq_socket_req_inproc, l, 4, 0)

  do while (l > 0) 
    rc = f77_zmq_send( zmq_socket_req_inproc, 'get_ao_integrals', 16, 0)

    do j = 1, l
      if (ao_overlap_abs(j,l) < ao_integrals_threshold) then
        cycle
      endif
      call compute_ao_integrals_jl(j,l,n_integrals,buffer_i,buffer_value) 
      rc = f77_zmq_send( zmq_socket_push_inproc, n_integrals, 4, ZMQ_SNDMORE)
      rc = f77_zmq_send( zmq_socket_push_inproc, buffer_i, key_kind*n_integrals, ZMQ_SNDMORE)
      rc = f77_zmq_send( zmq_socket_push_inproc, buffer_value, integral_kind*n_integrals, 0)
    enddo
    rc = f77_zmq_recv( zmq_socket_req_inproc, l, 4, 0)
  enddo

  deallocate( buffer_i, buffer_value )

  rc = f77_zmq_disconnect(zmq_socket_req_inproc, 'inproc://req_rep')
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
  character*(8), external        :: zmq_port
  integer(ZMQ_PTR)               :: zmq_socket_pull_inproc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)

  allocate ( buffer_i(ao_num*ao_num), buffer_value(ao_num*ao_num) )

  zmq_socket_pull_inproc = f77_zmq_socket(zmq_context, ZMQ_PULL)
  rc = f77_zmq_bind(zmq_socket_pull_inproc, 'inproc://push_pull')
  if (rc /= 0) then
    stop 'Unable to connect zmq_socket_pull_inproc'
  endif

  n_integrals = 0
  do while (n_integrals >= 0)
    
    rc = f77_zmq_recv( zmq_socket_pull_inproc, n_integrals, 4, 0)
    if (n_integrals > -1) then
      rc = f77_zmq_recv( zmq_socket_pull_inproc, buffer_i, key_kind*n_integrals, 0)
      rc = f77_zmq_recv( zmq_socket_pull_inproc, buffer_value, integral_kind*n_integrals, 0)
      call insert_into_ao_integrals_map(n_integrals,buffer_i,buffer_value)
    else
      rc = f77_zmq_recv( zmq_socket_pull_inproc, buffer_i, key_kind, 0)
      rc = f77_zmq_recv( zmq_socket_pull_inproc, buffer_value, integral_kind, 0)
    endif

  enddo

  rc = f77_zmq_unbind(zmq_socket_pull_inproc, 'inproc://push_pull')

  deallocate( buffer_i, buffer_value )
end

