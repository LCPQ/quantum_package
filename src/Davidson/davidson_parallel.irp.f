use bitmasks
use f77_zmq


subroutine davidson_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call davidson_run_slave(1,i)
end


subroutine davidson_slave_tcp(i)
  implicit none
  integer, intent(in)            :: i
  
  call davidson_run_slave(0,i)
end



subroutine davidson_run_slave(thread,iproc)
  use f77_zmq
  implicit none

  integer,  intent(in)           :: thread, iproc

  integer                        :: worker_id, task_id, blockb
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  if(worker_id == -1) then
    print *, "WORKER -1"
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    call end_zmq_push_socket(zmq_socket_push,thread)
    return
  end if
  
  integer :: sze_8
  integer, external :: align_double
  sze_8 = align_double(N_det)

  call davidson_slave_work(zmq_to_qp_run_socket, zmq_socket_push, N_states_diag, sze_8, worker_id)
  call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
end subroutine



subroutine davidson_slave_work(zmq_to_qp_run_socket, zmq_socket_push, N_st, sze_8, worker_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR),intent(in)   :: zmq_to_qp_run_socket
  integer(ZMQ_PTR),intent(in)   :: zmq_socket_push
  integer,intent(in)             :: worker_id, N_st, sze_8
  integer                        :: task_id
  character*(512)                :: msg
  integer                        :: imin, imax, ishift, istep
  
  double precision, allocatable  :: v_0(:,:), s_0(:,:), u_t(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: u_t, v_0, s_0

  allocate(v_0(N_det,N_st), s_0(N_det,N_st),u_t(N_st,N_det))

  ! Get wave function (u_t)
  ! -----------------------

  integer :: rc
  write(msg, *) 'get_psi ', worker_id
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)
  if (rc /= len(trim(msg))) then
    print *,  'f77_zmq_send(zmq_to_qp_run_socket,trim(msg),len(trim(msg)),0)'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,msg,len(msg),0)
  if (msg(1:13) /= 'get_psi_reply') then
    print *,  rc, trim(msg)
    print *,  'Error in get_psi_reply'
    stop 'error'
  endif

  integer                        :: N_states_read, N_det_read, psi_det_size_read
  integer                        :: N_det_selectors_read, N_det_generators_read
  double precision :: energy(N_states_diag)

  read(msg(14:rc),*) rc, N_states_read, N_det_read, psi_det_size_read,&
      N_det_generators_read, N_det_selectors_read
  if (rc /= worker_id) then
    print *,  'Wrong worker ID'
    stop 'error'
  endif
  
  if (N_states_read /= N_st) then
    stop 'error : N_st'
  endif

  if (N_det_read /= N_det) then
    stop 'error : N_det'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,psi_det,N_int*2*N_det*bit_kind,0)
  if (rc /= N_int*2*N_det*bit_kind) then
    print *, 'f77_zmq_recv(zmq_to_qp_run_socket,psi_det,N_int*2*N_det*bit_kind,0)'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,u_t,size(u_t)*8,0)
  if (rc /= size(u_t)*8) then
    print *,  rc, size(u_t)*8
    print *, 'f77_zmq_recv(zmq_to_qp_run_socket,u_t,size(u_t)×8,0)'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,energy,N_states_diag*8,0)
  if (rc /= N_states_diag*8) then
    print *, '77_zmq_recv(zmq_to_qp_run_socket,energy,N_states_diag*8,0)'
    stop 'error'
  endif

  ! Run tasks
  ! ---------

  do
    v_0 = 0.d0
    s_0 = 0.d0
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, msg)
    if(task_id == 0) exit
    read (msg,*) imin, imax, ishift, istep
    call H_S2_u_0_nstates_openmp_work(v_0,s_0,u_t,N_st,sze_8,imin,imax,ishift,istep)
    call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
    call davidson_push_results(zmq_socket_push, v_0, s_0, task_id)
  end do

end subroutine



subroutine davidson_push_results(zmq_socket_push, v_0, s_0, task_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)    :: zmq_socket_push
  integer             ,intent(in)    :: task_id
  double precision    ,intent(in)    :: v_0(N_det,N_states_diag)
  double precision    ,intent(in)    :: s_0(N_det,N_states_diag)
  integer                            :: rc

  rc = f77_zmq_send( zmq_socket_push, v_0, 8*N_states_diag*N_det, ZMQ_SNDMORE)
  if(rc /= 8*N_states_diag* N_det) stop "davidson_push_results failed to push vt"

  rc = f77_zmq_send( zmq_socket_push, s_0, 8*N_states_diag*N_det, ZMQ_SNDMORE)
  if(rc /= 8*N_states_diag* N_det) stop "davidson_push_results failed to push st"

  rc = f77_zmq_send( zmq_socket_push, task_id, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to push task_id"

! Activate is zmq_socket_push is a REQ
  integer :: idummy
  rc = f77_zmq_recv( zmq_socket_push, idummy, 4, 0)
  if (rc /= 4) then
    print *, irp_here, ': f77_zmq_send( zmq_socket_push, idummy, 4, 0)'
    stop 'error'
  endif

end subroutine



subroutine davidson_pull_results(zmq_socket_pull, v_0, s_0, task_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)     :: zmq_socket_pull
  integer             ,intent(out)    :: task_id
  double precision    ,intent(out)    :: v_0(N_det,N_states_diag)
  double precision    ,intent(out)    :: s_0(N_det,N_states_diag)

  integer                            :: rc

  rc = f77_zmq_recv( zmq_socket_pull, v_0, 8*size(v_0), 0)
  if(rc /= 8*size(s_0)) stop "davidson_push_results failed to pull v_0"

  rc = f77_zmq_recv( zmq_socket_pull, s_0, 8*size(s_0), 0)
  if(rc /= 8*size(s_0)) stop "davidson_push_results failed to pull s_0"

  rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)
  if(rc /= 4) stop "davidson_pull_results failed to pull task_id"

! Activate if zmq_socket_pull is a REP
  rc = f77_zmq_send( zmq_socket_pull, 0, 4, 0)
  if (rc /= 4) then
    print *,  irp_here, ' : f77_zmq_send (zmq_socket_pull,...'
    stop 'error'
  endif

end subroutine



subroutine davidson_collector(zmq_to_qp_run_socket, v0, s0, LDA)
  use f77_zmq
  implicit none

  integer                        :: LDA
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  
  double precision    ,intent(inout) :: v0(LDA, N_states_diag)
  double precision    ,intent(inout) :: s0(LDA, N_states_diag)
  
  integer                          :: more, task_id
  
  double precision, allocatable :: v_0(:,:), s_0(:,:)
  integer :: i,j
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull

  allocate(v_0(N_det,N_states_diag), s_0(N_det,N_states_diag))
  v0 = 0.d0 
  s0 = 0.d0 
  more = 1
  zmq_socket_pull = new_zmq_pull_socket()
  do while (more == 1)
    call davidson_pull_results(zmq_socket_pull, v_0, s_0, task_id)
    do j=1,N_states_diag
      do i=1,N_det
        v0(i,j) = v0(i,j) + v_0(i,j)
        s0(i,j) = s0(i,j) + s_0(i,j)
      enddo
    enddo
    call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more)
  end do
  deallocate(v_0,s_0)
  call end_zmq_pull_socket(zmq_socket_pull)

end subroutine




subroutine H_S2_u_0_nstates_zmq(v_0,s_0,u_0,N_st,sze_8)
  use omp_lib
  use bitmasks
  use f77_zmq
  implicit none
  BEGIN_DOC
  ! Computes v_0 = H|u_0> and s_0 = S^2 |u_0>
  !
  ! n : number of determinants
  !
  ! H_jj : array of <j|H|j>
  !
  ! S2_jj : array of <j|S^2|j>
  END_DOC
  integer, intent(in)            :: N_st, sze_8
  double precision, intent(out)  :: v_0(sze_8,N_st), s_0(sze_8,N_st)
  double precision, intent(inout):: u_0(sze_8,N_st)
  integer                        :: i,j,k
  integer                        :: ithread
  double precision, allocatable  :: u_t(:,:)
  !DIR$ ATTRIBUTES ALIGN : $IRP_ALIGN :: u_t
  
  allocate(u_t(N_st,N_det))
  do k=1,N_st
    call dset_order(u_0(1,k),psi_bilinear_matrix_order,N_det)
  enddo
  call dtranspose(                                                   &
      u_0,                                                           &
      size(u_0, 1),                                                  &
      u_t,                                                           &
      size(u_t, 1),                                                  &
      N_det, N_st)


  integer(ZMQ_PTR) :: zmq_to_qp_run_socket
  
  if(N_st /= N_states_diag .or. sze_8 < N_det) stop "assert fail in H_S2_u_0_nstates"

  ASSERT (Nint > 0)
  ASSERT (Nint == N_int)
  ASSERT (n>0)
  PROVIDE ref_bitmask_energy nproc

  v_0 = 0.d0
  s_0 = 0.d0
  
  call new_parallel_job(zmq_to_qp_run_socket,'davidson')
  
  character*(512) :: task
  integer :: rc
  double precision :: energy(N_st)
  energy = 0.d0

  task = ' '
  write(task,*) 'put_psi ', 1, N_st, N_det, N_det
  rc = f77_zmq_send(zmq_to_qp_run_socket,trim(task),len(trim(task)),ZMQ_SNDMORE)
  if (rc /= len(trim(task))) then
    print *, 'f77_zmq_send(zmq_to_qp_run_socket,trim(task),len(trim(task)),ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,psi_det,N_int*2*N_det*bit_kind,ZMQ_SNDMORE)
  if (rc /= N_int*2*N_det*bit_kind) then
    print *, 'f77_zmq_send(zmq_to_qp_run_socket,psi_det,N_int*2*N_det*bit_kind,ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,u_t,size(u_t)*8,ZMQ_SNDMORE)
  if (rc /= size(u_t)*8) then
    print *,  'f77_zmq_send(zmq_to_qp_run_socket,u_t,size(u_t)*8,ZMQ_SNDMORE)'
    stop 'error'
  endif

  rc = f77_zmq_send(zmq_to_qp_run_socket,energy,N_st*8,0)
  if (rc /= N_st*8) then
    print *, 'f77_zmq_send(zmq_to_qp_run_socket,energy,size_energy*8,0)'
    stop 'error'
  endif

  rc = f77_zmq_recv(zmq_to_qp_run_socket,task,len(task),0)
  if (task(1:rc) /= 'put_psi_reply 1') then
    print *,  rc, trim(task)
    print *,  'Error in put_psi_reply'
    stop 'error'
  endif

  deallocate(u_t)


  integer :: istep, imin, imax, ishift
  istep=2
  do imin=1,N_det, 524288
    do ishift=0,istep-1
      imax = min(N_det, imin+524288-1)
      write(task,'(4(I9,1X),1A)') imin, imax, ishift, istep, '|'
      call add_task_to_taskserver(zmq_to_qp_run_socket,trim(task))
    enddo
  enddo

  v_0 = 0.d0
  s_0 = 0.d0

  call omp_set_nested(.True.)
  !$OMP PARALLEL NUM_THREADS(2) PRIVATE(ithread)
  ithread = omp_get_thread_num()
  if (ithread == 0 ) then
    call zmq_set_running(zmq_to_qp_run_socket)
    call davidson_collector(zmq_to_qp_run_socket, v_0, s_0, size(v_0,1))
  else 
    call davidson_slave_inproc(1)
  endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'davidson')

  do k=1,N_st
    call dset_order(v_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
    call dset_order(s_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
    call dset_order(u_0(1,k),psi_bilinear_matrix_order_reverse,N_det)
  enddo
end

