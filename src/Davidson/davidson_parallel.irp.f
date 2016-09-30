
!brought to you by garniroy inc.

use bitmasks

subroutine davidson_process(block, N, idx, vt, st)
  
  implicit none
  

  integer             , intent(in)        :: block
  integer             , intent(inout)     :: N
  integer             , intent(inout)     :: idx(N_det)
  double precision    , intent(inout)     :: vt(N_states, N_det)
  double precision    , intent(inout)     :: st(N_states, N_det)
  
  integer :: i, j, sh, sh2, exa, ext, org_i, org_j, istate, ni, endi
  integer(bit_kind) :: sorted_i(N_int)
  double precision :: s2, hij
  
  vt = 0d0
  st = 0d0
  
  N = N_det
  do i=1,N
    idx(i) = i
  end do
  
  sh = block
  
  do sh2=1,sh
    exa = 0
    do ni=1,N_int
      exa = exa + popcnt(xor(version_(ni,sh,1), version_(ni,sh2,1)))
    end do
    if(exa > 2) then
      cycle
    end if
    
    do i=shortcut_(sh,1),shortcut_(sh+1,1)-1
      org_i = sort_idx_(i,1)
      if(sh==sh2) then
        endi = i-1
      else
        endi = shortcut_(sh2+1,1)-1
      end if
      do ni=1,N_int
        sorted_i(ni) = sorted_(ni,i,1)
      enddo
      
      do j=shortcut_(sh2,1),endi
        org_j = sort_idx_(j,1)
        ext = exa
        do ni=1,N_int
          ext = ext + popcnt(xor(sorted_i(ni), sorted_(ni,j,1)))
        end do
        if(ext <= 4) then
          call i_h_j (psi_det(1,1,org_j),psi_det(1,1,org_i),n_int,hij)
          call get_s2(psi_det(1,1,org_j),psi_det(1,1,org_i),n_int,s2) 
          do istate=1,N_states
            vt (istate,org_i) = vt (istate,org_i) + hij*ut(istate,org_j)
            vt (istate,org_j) = vt (istate,org_j) + hij*ut(istate,org_i)
            st (istate,org_i) = st (istate,org_i) + s2*ut(istate,org_j)
            st (istate,org_j) = st (istate,org_j) + s2*ut(istate,org_i)
          enddo
        endif
      enddo
    enddo
  enddo
end subroutine


 BEGIN_PROVIDER [ integer, shortcut_, (0:N_det+1, 2) ]
&BEGIN_PROVIDER [ integer(bit_kind), version_, (N_int, N_det, 2) ]
&BEGIN_PROVIDER [ integer(bit_kind), sorted_, (N_int, N_det, 2) ]
&BEGIN_PROVIDER [ integer, sort_idx_, (N_det, 2) ]
  implicit none
  call sort_dets_ab_v(psi_det, sorted_(1,1,1), sort_idx_(1,1), shortcut_(0,1), version_(1,1,1), n_det, N_int)
  call sort_dets_ba_v(psi_det, sorted_(1,1,2), sort_idx_(1,2), shortcut_(0,2), version_(1,1,2), n_det, N_int)
END_PROVIDER



subroutine davidson_collect(block, N, idx, vt, st , v0, s0)
  implicit none


  integer             , intent(in)        :: block
  integer             , intent(in)        :: N
  integer             , intent(in)        :: idx(N)
  double precision    , intent(in)        :: vt(N_states, N)
  double precision    , intent(in)        :: st(N_states, N)
  double precision    , intent(inout)     :: v0(N_det, N_states)
  double precision    , intent(inout)     :: s0(N_det, N_states)

  integer :: i

  do i=1,N
    v0(idx(i), :) += vt(:, i)
    s0(idx(i), :) += st(:, i)
  end do
end subroutine


subroutine davidson_init(zmq_to_qp_run_socket)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR), intent(out) :: zmq_to_qp_run_socket ! zmq_to_qp_run_socket
  
  call new_parallel_job(zmq_to_qp_run_socket,'davidson')
end subroutine



subroutine davidson_add_task(zmq_to_qp_run_socket, block)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)    :: zmq_to_qp_run_socket
  integer             ,intent(in)    :: block
  character*(512)                    :: task 
  
  
  write(task,*) block
  call add_task_to_taskserver(zmq_to_qp_run_socket, task)
end subroutine



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

  integer                        :: worker_id, task_id, block
  character*(512)                :: task

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
  
  call davidson_slave_work(zmq_to_qp_run_socket, zmq_socket_push, worker_id)
  
  call disconnect_from_taskserver(zmq_to_qp_run_socket,zmq_socket_push,worker_id)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
end subroutine



subroutine davidson_slave_work(zmq_to_qp_run_socket, zmq_socket_push, worker_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR),intent(in)   :: zmq_to_qp_run_socket
  integer(ZMQ_PTR),intent(in)   :: zmq_socket_push
  integer,intent(in)            :: worker_id
  integer :: task_id
  character*(512) :: task
  

  integer                  :: block
  integer                  :: N
  integer             ,  allocatable      :: idx(:)
  double precision    ,  allocatable      :: vt(:,:)
  double precision    ,  allocatable      :: st(:,:)
  
  
  allocate(idx(N_det)) 
  allocate(vt(N_states, N_det)) 
  allocate(st(N_states, N_det)) 
  
  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task)
    if(task_id == 0) exit
    read (task,*) block
    
    call davidson_process(block,N, idx, vt, st)
    
    call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
    call davidson_push_results(zmq_socket_push, block, N, idx, vt, st, task_id)
  end do
end subroutine



subroutine davidson_push_results(zmq_socket_push, block, N, idx, vt, st, task_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)    :: zmq_socket_push
  integer             ,intent(in)    :: task_id

  integer             ,intent(in)    :: block
  integer             ,intent(in)    :: N
  integer             ,intent(in)    :: idx(N)
  double precision    ,intent(in)    :: vt(N_states, N)
  double precision    ,intent(in)    :: st(N_states, N)
  integer                            :: rc

  rc = f77_zmq_send( zmq_socket_push, block, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "davidson_push_results failed to push block"

  rc = f77_zmq_send( zmq_socket_push, N, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "davidson_push_results failed to push N"

  rc = f77_zmq_send( zmq_socket_push, idx, 4*N, ZMQ_SNDMORE)
  if(rc /= 4*N) stop "davidson_push_results failed to push idx"

  rc = f77_zmq_send( zmq_socket_push, vt, 8*N_states* N, ZMQ_SNDMORE)
  if(rc /= 8*N_states* N) stop "davidson_push_results failed to push vt"

  rc = f77_zmq_send( zmq_socket_push, st, 8*N_states* N, ZMQ_SNDMORE)
  if(rc /= 8*N_states* N) stop "davidson_push_results failed to push st"

  rc = f77_zmq_send( zmq_socket_push, task_id, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to push task_id"
end subroutine



subroutine davidson_pull_results(zmq_socket_pull, block, N, idx, vt, st, task_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)     :: zmq_socket_pull
  integer             ,intent(out)    :: task_id
  integer             ,intent(out)    :: block
  integer             ,intent(out)    :: N
  integer             ,intent(out)    :: idx(N_det)
  double precision    ,intent(out)    :: vt(N_states, N_det)
  double precision    ,intent(out)    :: st(N_states, N_det)

  integer                            :: rc

  rc = f77_zmq_recv( zmq_socket_pull, block, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to pull block"

  rc = f77_zmq_recv( zmq_socket_pull, N, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to pull N"

  rc = f77_zmq_recv( zmq_socket_pull, idx, 4*N, 0)
  if(rc /= 4*N) stop "davidson_push_results failed to pull idx"

  rc = f77_zmq_recv( zmq_socket_pull, vt, 8*N_states* N, 0)
  if(rc /= 8*N_states* N) stop "davidson_push_results failed to pull vt"

  rc = f77_zmq_recv( zmq_socket_pull, st, 8*N_states* N, 0)
  if(rc /= 8*N_states* N) stop "davidson_push_results failed to pull st"

  rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)
  if(rc /= 4) stop "davidson_pull_results failed to pull task_id"
end subroutine



subroutine davidson_collector(zmq_to_qp_run_socket, zmq_socket_pull , v0, s0)
  use f77_zmq
  implicit none

  integer(ZMQ_PTR), intent(in)     :: zmq_to_qp_run_socket
  integer(ZMQ_PTR), intent(in)     :: zmq_socket_pull

  double precision    ,intent(inout)    :: v0(N_det, N_states)
  double precision    ,intent(inout)    :: s0(N_det, N_states)

  integer                          :: more, task_id
  

  integer                  :: block
  integer                  :: N
  integer             , allocatable      :: idx(:)
  double precision    , allocatable      :: vt(:,:)
  double precision    , allocatable      :: st(:,:)
  
  
  allocate(idx(N_det)) 
  allocate(vt(N_states, N_det)) 
  allocate(st(N_states, N_det)) 
  
  more = 1
  
  do while (more == 1)
    call davidson_pull_results(zmq_socket_pull, block, N, idx, vt, st, task_id)
    call davidson_collect(block, N, idx, vt, st , v0, s0)
    
    call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more)
  end do
end subroutine


subroutine davidson_run(zmq_to_qp_run_socket , v0, s0)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR), intent(in) :: zmq_to_qp_run_socket
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)             :: zmq_collector
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull
  
  integer :: i
  integer, external              :: omp_get_thread_num

  double precision    , intent(inout)     :: v0(N_det, N_states)
  double precision    , intent(inout)     :: s0(N_det, N_states)
  
  call zmq_set_running(zmq_to_qp_run_socket)
  
  zmq_collector = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  i = omp_get_thread_num()
  
  
  PROVIDE nproc
  
  !$OMP PARALLEL DEFAULT(shared) private(i) num_threads(nproc+1)
    i = omp_get_thread_num()
    if (i==0) then
      call davidson_collector(zmq_collector, zmq_socket_pull , v0, s0)
      call end_zmq_to_qp_run_socket(zmq_collector)
      call end_zmq_pull_socket(zmq_socket_pull)
    else
      call davidson_slave_inproc(i)
    endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'davidson')
end subroutine



  

