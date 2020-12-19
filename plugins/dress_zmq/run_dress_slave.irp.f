BEGIN_PROVIDER [ integer, fragment_count ]
  implicit none
  BEGIN_DOC
    ! Number of fragments for the deterministic part
  END_DOC
  fragment_count = 1
END_PROVIDER


subroutine run_dress_slave(thread,iproc,energy)
  use f77_zmq
  implicit none

  double precision, intent(in)    :: energy(N_states_diag)
  integer,  intent(in)            :: thread, iproc
  integer                        :: rc, i, subset, i_generator

  integer                        :: worker_id, task_id, ctask, ltask
  character*(512)                :: task

  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_push_socket
  integer(ZMQ_PTR)               :: zmq_socket_push

  logical :: done

  double precision,allocatable :: dress_detail(:)
  integer :: ind
  
  double precision,allocatable :: delta_ij_loc(:,:,:)
  double precision :: div(N_states) 
  integer :: h,p,n,i_state
  logical :: ok

  allocate(delta_ij_loc(N_states,N_det,2)) 
  
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  if(worker_id == -1) then
    print *, "WORKER -1"
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    call end_zmq_push_socket(zmq_socket_push,thread)
    return
  end if
  do i=1,N_states
    div(i) = psi_coef(dressed_column_idx(i), i)
  end do
  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task)
    
    if(task_id /= 0) then
      read (task,*) subset, i_generator
      delta_ij_loc = 0d0
      call alpha_callback(delta_ij_loc, i_generator, subset, iproc)

      call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
      call push_dress_results(zmq_socket_push, i_generator, delta_ij_loc, task_id)
    else
      exit
    end if
  end do
  call disconnect_from_taskserver(zmq_to_qp_run_socket,worker_id)
  call end_zmq_push_socket(zmq_socket_push,thread)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
end subroutine


subroutine push_dress_results(zmq_socket_push, ind, delta_loc, task_id)
  use f77_zmq
  implicit none

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  double precision, intent(in)   :: delta_loc(N_states, N_det, 2)
  integer, intent(in) :: ind, task_id
  integer :: rc, i
  

  rc = f77_zmq_send( zmq_socket_push, ind, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"


  rc = f77_zmq_send( zmq_socket_push, delta_loc, 8*N_states*N_det*2, ZMQ_SNDMORE)
  if(rc /= 8*N_states*N_det*2) stop "push"
  
  rc = f77_zmq_send( zmq_socket_push, task_id, 4, 0)
  if(rc /= 4) stop "push"

! Activate is zmq_socket_push is a REQ
IRP_IF ZMQ_PUSH
IRP_ELSE
  character*(2) :: ok
  rc = f77_zmq_recv( zmq_socket_push, ok, 2, 0)
IRP_ENDIF

end subroutine


subroutine pull_dress_results(zmq_socket_pull, ind, delta_loc, task_id)
  use f77_zmq
  implicit none
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  double precision, intent(inout) :: delta_loc(N_states, N_det, 2)
  integer, intent(out) :: ind
  integer, intent(out) :: task_id
  integer :: rc, i


  rc = f77_zmq_recv( zmq_socket_pull, ind, 4, 0)
  if(rc /= 4) stop "pull"
  
  rc = f77_zmq_recv( zmq_socket_pull, delta_loc, N_states*8*N_det*2, 0)
  if(rc /= 8*N_states*N_det*2) stop "pull"

  rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)
  if(rc /= 4) stop "pull"

! Activate is zmq_socket_pull is a REP
IRP_IF ZMQ_PUSH
IRP_ELSE
  rc = f77_zmq_send( zmq_socket_pull, 'ok', 2, 0)
IRP_ENDIF

end subroutine
 
 
            
