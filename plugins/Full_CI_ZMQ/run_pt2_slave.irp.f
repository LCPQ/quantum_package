
subroutine run_pt2_slave(thread,iproc,energy)
  use f77_zmq
  use selection_types
  implicit none

  double precision, intent(in)    :: energy(N_states_diag)
  integer,  intent(in)            :: thread, iproc
  integer                        :: rc, i

  integer                        :: worker_id, task_id(1), ctask, ltask
  character*(512)                :: task

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  type(selection_buffer) :: buf
  logical :: done

  double precision :: pt2(N_states)
  double precision,allocatable :: pt2_detail(:,:)
  integer :: index
  integer :: Nindex

  allocate(pt2_detail(N_states, N_det_generators))
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  if(worker_id == -1) then
    print *, "WORKER -1"
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    call end_zmq_push_socket(zmq_socket_push,thread)
    return
  end if
  buf%N = 0
  ctask = 1
  Nindex=1
  pt2 = 0d0
  pt2_detail = 0d0
  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id(ctask), task)

    done = task_id(ctask) == 0
    if (done) then
      ctask = ctask - 1
    else
      integer :: i_generator, i_i_generator, subset
      read (task,*) subset, index
      
      if(buf%N == 0) then
        ! Only first time 
        call create_selection_buffer(1, 2, buf)
      end if
      do i_i_generator=1, Nindex
        i_generator = index
        call select_connected(i_generator,energy,pt2_detail(1, i_i_generator),buf,subset)
        pt2(:) += pt2_detail(:, i_generator)
      enddo
    endif

    if(done .or. (ctask == size(task_id)) ) then
      if(buf%N == 0 .and. ctask > 0) stop "uninitialized selection_buffer"
      do i=1, ctask
         call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id(i))
      end do
      if(ctask > 0) then
        call push_pt2_results(zmq_socket_push, Nindex, index, pt2_detail, task_id(1), ctask)
        pt2 = 0d0
        pt2_detail(:,:Nindex) = 0d0
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
  call delete_selection_buffer(buf)
end subroutine


subroutine push_pt2_results(zmq_socket_push, N, index, pt2_detail, task_id, ntask)
  use f77_zmq
  use selection_types
  implicit none

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  double precision, intent(in)   :: pt2_detail(N_states, N_det_generators)
  integer, intent(in) :: ntask, N, index, task_id(*)
  integer :: rc


  rc = f77_zmq_send( zmq_socket_push, N, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"

  rc = f77_zmq_send( zmq_socket_push, index, 4, ZMQ_SNDMORE)
  if(rc /= 4*N) stop "push"


  rc = f77_zmq_send( zmq_socket_push, pt2_detail, 8*N_states*N, ZMQ_SNDMORE)
  if(rc /= 8*N_states*N) stop "push"

  rc = f77_zmq_send( zmq_socket_push, ntask, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"

  rc = f77_zmq_send( zmq_socket_push, task_id, ntask*4, 0)
  if(rc /= 4*ntask) stop "push"

! Activate is zmq_socket_push is a REQ
IRP_IF ZMQ_PUSH
IRP_ELSE
  character*(2) :: ok
  rc = f77_zmq_recv( zmq_socket_push, ok, 2, 0)
IRP_ENDIF

end subroutine


subroutine pull_pt2_results(zmq_socket_pull, N, index, pt2_detail, task_id, ntask)
  use f77_zmq
  use selection_types
  implicit none
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  double precision, intent(inout) :: pt2_detail(N_states, N_det_generators)
  integer, intent(out) :: index
  integer, intent(out) :: N, ntask, task_id(*)
  integer :: rc, rn, i

  rc = f77_zmq_recv( zmq_socket_pull, N, 4, 0)
  if(rc /= 4) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, index, 4, 0)
  if(rc /= 4*N) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, pt2_detail, N_states*8*N, 0)
  if(rc /= 8*N_states*N) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, ntask, 4, 0)
  if(rc /= 4) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, task_id, ntask*4, 0)
  if(rc /= 4*ntask) stop "pull"

! Activate is zmq_socket_pull is a REP
IRP_IF ZMQ_PUSH
IRP_ELSE
  rc = f77_zmq_send( zmq_socket_pull, 'ok', 2, 0)
IRP_ENDIF

end subroutine
 
 
BEGIN_PROVIDER [ double precision, pt2_workload, (N_det_generators) ]
  integer :: i
  do i=1,N_det_generators
    pt2_workload(i) = dfloat(N_det_generators - i + 1)**2
  end do
  pt2_workload = pt2_workload / sum(pt2_workload)
END_PROVIDER
            
