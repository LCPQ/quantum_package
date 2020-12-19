
subroutine run_pt2_slave(thread,iproc,energy)
  use f77_zmq
  use selection_types
  implicit none

  double precision, intent(in)    :: energy(N_states_diag)
  integer,  intent(in)            :: thread, iproc
  integer                        :: rc, i

  integer                        :: worker_id, ctask, ltask
  character*(512), allocatable   :: task(:)
  integer, allocatable           :: task_id(:)

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  type(selection_buffer) :: buf
  logical :: done

  double precision,allocatable :: pt2(:,:)
  integer :: n_tasks, k, n_tasks_max
  integer, allocatable :: i_generator(:), subset(:)

  n_tasks_max = N_det_generators/100+1
  allocate(task_id(n_tasks_max), task(n_tasks_max))
  allocate(pt2(N_states,n_tasks_max), i_generator(n_tasks_max), subset(n_tasks_max))

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()

  integer, external :: connect_to_taskserver
  if (connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread) == -1) then
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    return
  endif
           
  zmq_socket_push      = new_zmq_push_socket(thread)

  buf%N = 0
  n_tasks = 0
  call create_selection_buffer(0, 0, buf)

  done = .False.
  do while (.not.done)

    n_tasks = min(n_tasks+1,n_tasks_max)

    integer, external :: get_tasks_from_taskserver
    if (get_tasks_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task, n_tasks) == -1) then
      exit
    endif
    done = task_id(n_tasks) == 0
    if (done) n_tasks = n_tasks-1
    if (n_tasks == 0) exit

    do k=1,n_tasks
      read (task(k),*) subset(k), i_generator(k)
    enddo

    do k=1,n_tasks
        pt2(:,k) = 0.d0
        buf%cur = 0
        call select_connected(i_generator(k),energy,pt2(1,k),buf,subset(k))
    enddo
    integer, external :: tasks_done_to_taskserver
    if (tasks_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id,n_tasks) == -1) then
      done = .true.
    endif
    call push_pt2_results(zmq_socket_push, i_generator, pt2, task_id, n_tasks)
  end do

  integer, external :: disconnect_from_taskserver
  if (disconnect_from_taskserver(zmq_to_qp_run_socket,worker_id) == -1) then 
    continue 
  endif 

  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
  call delete_selection_buffer(buf)
end subroutine


subroutine push_pt2_results(zmq_socket_push, index, pt2, task_id, n_tasks)
  use f77_zmq
  use selection_types
  implicit none

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  double precision, intent(in)   :: pt2(N_states,n_tasks)
  integer, intent(in) :: n_tasks, index(n_tasks), task_id(n_tasks)
  integer :: rc

  rc = f77_zmq_send( zmq_socket_push, n_tasks, 4, ZMQ_SNDMORE)
  if (rc == -1) then
    return
  endif
  if(rc /= 4) stop 'push'


  rc = f77_zmq_send( zmq_socket_push, index, 4*n_tasks, ZMQ_SNDMORE)
  if (rc == -1) then
    return
  endif
  if(rc /= 4*n_tasks) stop 'push'


  rc = f77_zmq_send( zmq_socket_push, pt2, 8*N_states*n_tasks, ZMQ_SNDMORE)
  if (rc == -1) then
    return
  endif
  if(rc /= 8*N_states*n_tasks) stop 'push'

  rc = f77_zmq_send( zmq_socket_push, task_id, n_tasks*4, 0)
  if (rc == -1) then
    return
  endif
  if(rc /= 4*n_tasks) stop 'push'

! Activate is zmq_socket_push is a REQ
IRP_IF ZMQ_PUSH
IRP_ELSE
  character*(2) :: ok
  rc = f77_zmq_recv( zmq_socket_push, ok, 2, 0)
  if (rc == -1) then
    return
  endif
  if ((rc /= 2).and.(ok(1:2) /= 'ok')) then
    print *,  irp_here//': error in receiving ok'
    stop -1
  endif
IRP_ENDIF

end subroutine


subroutine pull_pt2_results(zmq_socket_pull, index, pt2, task_id, n_tasks)
  use f77_zmq
  use selection_types
  implicit none
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  double precision, intent(inout) :: pt2(N_states,*)
  integer, intent(out) :: index(*)
  integer, intent(out) :: n_tasks, task_id(*)
  integer :: rc, rn, i

  rc = f77_zmq_recv( zmq_socket_pull, n_tasks, 4, 0)
  if (rc == -1) then
    n_tasks = 1
    task_id(1) = 0
  endif
  if(rc /= 4) stop 'pull'

  rc = f77_zmq_recv( zmq_socket_pull, index, 4*n_tasks, 0)
  if (rc == -1) then
    n_tasks = 1
    task_id(1) = 0
  endif
  if(rc /= 4*n_tasks) stop 'pull'

  rc = f77_zmq_recv( zmq_socket_pull, pt2, N_states*8*n_tasks, 0)
  if (rc == -1) then
    n_tasks = 1
    task_id(1) = 0
  endif
  if(rc /= 8*N_states*n_tasks) stop 'pull'

  rc = f77_zmq_recv( zmq_socket_pull, task_id, n_tasks*4, 0)
  if (rc == -1) then
    n_tasks = 1
    task_id(1) = 0
  endif
  if(rc /= 4*n_tasks) stop 'pull'

! Activate is zmq_socket_pull is a REP
IRP_IF ZMQ_PUSH
IRP_ELSE
  rc = f77_zmq_send( zmq_socket_pull, 'ok', 2, 0)
  if (rc == -1) then
    n_tasks = 1
    task_id(1) = 0
  endif
  if (rc /= 2) then
    print *,  irp_here//': error in sending ok'
    stop -1
  endif
IRP_ENDIF

end subroutine
 
 
BEGIN_PROVIDER [ double precision, pt2_workload, (N_det_generators) ]
  integer :: i
  do i=1,N_det_generators
    pt2_workload(i) = dfloat(N_det_generators - i + 1)**2
  end do
  pt2_workload = pt2_workload / sum(pt2_workload)
END_PROVIDER
            
