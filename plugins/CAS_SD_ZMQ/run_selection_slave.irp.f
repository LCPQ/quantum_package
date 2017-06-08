
subroutine run_selection_slave(thread,iproc,energy)
  use f77_zmq
  use selection_types
  implicit none

  double precision, intent(in)    :: energy(N_states)
  integer,  intent(in)            :: thread, iproc
  integer                        :: rc, i

  integer                        :: worker_id, task_id(1), ctask, ltask
  character*(512)                :: task

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  type(selection_buffer) :: buf, buf2
  logical :: done
  double precision :: pt2(N_states)

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  if(worker_id == -1) then
    print *, "WORKER -1"
    !call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    call end_zmq_push_socket(zmq_socket_push,thread)
    return
  end if
  buf%N = 0
  ctask = 1
  pt2 = 0d0

  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id(ctask), task)
    done = task_id(ctask) == 0
    if (done) then
      ctask = ctask - 1
    else
      integer :: i_generator, N
      read (task,*) i_generator, N
      if(buf%N == 0) then
        ! Only first time 
        call create_selection_buffer(N, N*2, buf)
        call create_selection_buffer(N, N*3, buf2)
      else
        if(N /= buf%N) stop "N changed... wtf man??"
      end if
      call select_connected(i_generator,energy,pt2,buf)
    endif

    if(done .or. ctask == size(task_id)) then
      if(buf%N == 0 .and. ctask > 0) stop "uninitialized selection_buffer"
      do i=1, ctask
         call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id(i))
      end do
      if(ctask > 0) then
        call push_selection_results(zmq_socket_push, pt2, buf, task_id(1), ctask)
        do i=1,buf%cur
          call add_to_selection_buffer(buf2, buf%det(1,1,i), buf%val(i))
        enddo
        call sort_selection_buffer(buf2)
        buf%mini = buf2%mini
        pt2 = 0d0
        buf%cur = 0
      end if
      ctask = 0
    end if

    if(done) exit
    ctask = ctask + 1
  end do
  call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
end subroutine


subroutine push_selection_results(zmq_socket_push, pt2, b, task_id, ntask)
  use f77_zmq
  use selection_types
  implicit none

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  double precision, intent(in)   :: pt2(N_states)
  type(selection_buffer), intent(inout) :: b
  integer, intent(in) :: ntask, task_id(*)
  integer :: rc

  call sort_selection_buffer(b)

  rc = f77_zmq_send( zmq_socket_push, b%cur, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"
  rc = f77_zmq_send( zmq_socket_push, pt2, 8*N_states, ZMQ_SNDMORE)
  if(rc /= 8*N_states) stop "push"

  rc = f77_zmq_send( zmq_socket_push, b%val(1), 8*b%cur, ZMQ_SNDMORE)
  if(rc /= 8*b%cur) stop "push"

  rc = f77_zmq_send( zmq_socket_push, b%det(1,1,1), bit_kind*N_int*2*b%cur, ZMQ_SNDMORE)
  if(rc /= bit_kind*N_int*2*b%cur) stop "push"

  rc = f77_zmq_send( zmq_socket_push, ntask, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"

  rc = f77_zmq_send( zmq_socket_push, task_id(1), ntask*4, 0)
  if(rc /= 4*ntask) stop "push"

! Activate is zmq_socket_push is a REQ
IRP_IF ZMQ_PUSH
IRP_ELSE
  rc = f77_zmq_recv( zmq_socket_push, task_id(1), ntask*4, 0)
IRP_ENDIF

end subroutine


subroutine pull_selection_results(zmq_socket_pull, pt2, val, det, N, task_id, ntask)
  use f77_zmq
  use selection_types
  implicit none
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  double precision, intent(inout) :: pt2(N_states)
  double precision, intent(out) :: val(*)
  integer(bit_kind), intent(out) :: det(N_int, 2, *)
  integer, intent(out) :: N, ntask, task_id(*)
  integer :: rc, rn, i

  rc = f77_zmq_recv( zmq_socket_pull, N, 4, 0)
  if(rc /= 4) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, pt2, N_states*8, 0)
  if(rc /= 8*N_states) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, val(1), 8*N, 0)
  if(rc /= 8*N) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, det(1,1,1), bit_kind*N_int*2*N, 0)
  if(rc /= bit_kind*N_int*2*N) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, ntask, 4, 0)
  if(rc /= 4) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, task_id(1), ntask*4, 0)
  if(rc /= 4*ntask) stop "pull"

! Activate is zmq_socket_pull is a REP
IRP_IF ZMQ_PUSH
IRP_ELSE
  rc = f77_zmq_send( zmq_socket_pull, task_id(1), ntask*4, 0)
IRP_ENDIF

end subroutine
 
 

