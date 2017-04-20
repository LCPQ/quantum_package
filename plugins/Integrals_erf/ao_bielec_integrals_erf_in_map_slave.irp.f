subroutine ao_bielec_integrals_erf_in_map_slave_tcp(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
! Computes a buffer of integrals. i is the ID of the current thread.
  END_DOC
  call ao_bielec_integrals_erf_in_map_slave(0,i)
end


subroutine ao_bielec_integrals_erf_in_map_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
! Computes a buffer of integrals. i is the ID of the current thread.
  END_DOC
  call ao_bielec_integrals_erf_in_map_slave(1,i)
end



subroutine ao_bielec_integrals_erf_in_map_slave(thread,iproc)
  use map_module
  use f77_zmq
  implicit none
  BEGIN_DOC
! Computes a buffer of integrals
  END_DOC

  integer, intent(in)            :: thread, iproc

  integer                        :: j,l,n_integrals
  integer                        :: rc
  real(integral_kind), allocatable :: buffer_value(:)
  integer(key_kind), allocatable :: buffer_i(:)
  
  integer                        :: worker_id, task_id
  character*(512)                :: task

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  character*(64)                 :: state

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)

  allocate ( buffer_i(ao_num*ao_num), buffer_value(ao_num*ao_num) )

  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)

  do 
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task)
    if (task_id == 0) exit
    read(task,*) j, l
    call compute_ao_integrals_erf_jl(j,l,n_integrals,buffer_i,buffer_value) 
    call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
    call push_integrals(zmq_socket_push, n_integrals, buffer_i, buffer_value, task_id)
  enddo


  call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
  deallocate( buffer_i, buffer_value )
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)

end


subroutine ao_bielec_integrals_erf_in_map_collector
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
  
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket
  
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull
  
  integer*8                      :: control, accu
  integer                        :: task_id, more, sze

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()

  sze = ao_num*ao_num
  allocate ( buffer_i(sze), buffer_value(sze) )

  accu = 0_8
  more = 1
  do while (more == 1)
    
    rc = f77_zmq_recv( zmq_socket_pull, n_integrals, 4, 0)
    if (rc == -1) then
      n_integrals = 0
      return
    endif
    if (rc /= 4) then
      print *, irp_here,  ': f77_zmq_recv( zmq_socket_pull, n_integrals, 4, 0)'
      stop 'error'
    endif
    
    if (n_integrals >= 0) then
      
      if (n_integrals > sze) then
        deallocate (buffer_value, buffer_i)
        sze = n_integrals
        allocate (buffer_value(sze), buffer_i(sze))
      endif

      rc = f77_zmq_recv( zmq_socket_pull, buffer_i, key_kind*n_integrals, 0)
      if (rc /= key_kind*n_integrals) then
        print *,  rc, key_kind, n_integrals
        print *, irp_here,  ': f77_zmq_recv( zmq_socket_pull, buffer_i, key_kind*n_integrals, 0)'
        stop 'error'
      endif
      
      rc = f77_zmq_recv( zmq_socket_pull, buffer_value, integral_kind*n_integrals, 0)
      if (rc /= integral_kind*n_integrals) then
        print *, irp_here,  ': f77_zmq_recv( zmq_socket_pull, buffer_value, integral_kind*n_integrals, 0)'
        stop 'error'
      endif

      rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)

! Activate if zmq_socket_pull is a REP
      rc = f77_zmq_send( zmq_socket_pull, 0, 4, 0)
      if (rc /= 4) then
        print *,  irp_here, ' : f77_zmq_send (zmq_socket_pull,...'
        stop 'error'
      endif

      
      call insert_into_ao_integrals_erf_map(n_integrals,buffer_i,buffer_value)
      accu += n_integrals
      if (task_id /= 0) then
        call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more)
      endif
    endif

  enddo

  deallocate( buffer_i, buffer_value )

  integer (map_size_kind) :: get_ao_erf_map_size 
  control = get_ao_erf_map_size(ao_integrals_erf_map)

  if (control /= accu) then
      print *, ''
      print *, irp_here
      print *, 'Control : ', control
      print *, 'Accu    : ', accu
      print *, 'Some integrals were lost during the parallel computation.'
      print *, 'Try to reduce the number of threads.'
      stop
  endif

  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)

end

