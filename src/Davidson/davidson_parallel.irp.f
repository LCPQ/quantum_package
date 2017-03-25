
!brought to you by garniroy inc.

use bitmasks
use f77_zmq

subroutine davidson_process(blockb, blockb2, N, idx, vt, st, bs, istep)
  
  implicit none
  

  integer             , intent(in)        :: blockb, bs, blockb2, istep
  integer             , intent(inout)     :: N
  integer             , intent(inout)     :: idx(bs)
  double precision    , intent(inout)     :: vt(N_states_diag, bs)
  double precision    , intent(inout)     :: st(N_states_diag, bs)
  
  integer :: i,ii, j, sh, sh2, exa, ext, org_i, org_j, istate, ni, endi
  integer(bit_kind) :: sorted_i(N_int)
  double precision :: s2, hij
  logical, allocatable :: wrotten(:)
  
  PROVIDE dav_det ref_bitmask_energy

  allocate(wrotten(bs))
  wrotten = .false.

  ii=0
  sh = blockb
  do sh2=1,shortcut_(0,1)
    exa = popcnt(xor(version_(1,sh,1), version_(1,sh2,1)))
    do ni=2,N_int
      exa = exa + popcnt(xor(version_(ni,sh,1), version_(ni,sh2,1)))
    end do
    if(exa > 2) cycle
    
    do i=blockb2+shortcut_(sh,1),shortcut_(sh+1,1)-1, istep
      ii = i - shortcut_(blockb,1) + 1

      org_i =  sort_idx_(i,1)
      do ni=1,N_int
        sorted_i(ni) = sorted_(ni,i,1)
      enddo
      
      do j=shortcut_(sh2,1), shortcut_(sh2+1,1)-1
        if(i == j) cycle
        ext = exa + popcnt(xor(sorted_i(1), sorted_(1,j,1)))
        if(ext > 4) cycle
        do ni=2,N_int
          ext = ext + popcnt(xor(sorted_i(ni), sorted_(ni,j,1)))
          if(ext > 4) exit
        end do
        if(ext <= 4) then
          org_j = sort_idx_(j,1)
          call i_h_j (dav_det(1,1,org_j),dav_det(1,1,org_i),n_int,hij)
          call get_s2(dav_det(1,1,org_j),dav_det(1,1,org_i),n_int,s2) 
!          call i_h_j (sorted_(1,j,1),sorted_(1,i,1),n_int,hij)
!          call get_s2(sorted_(1,j,1),sorted_(1,i,1),n_int,s2) 
          if(.not. wrotten(ii)) then
            wrotten(ii) = .true.
            idx(ii) = org_i
            vt (:,ii) = 0d0
            st (:,ii) = 0d0
          end if
          do istate=1,N_states_diag
            vt (istate,ii) = vt (istate,ii) +hij*dav_ut(istate,org_j)
            st (istate,ii) = st (istate,ii) +s2*dav_ut(istate,org_j)
          enddo
        endif
      enddo
    enddo
  enddo
  

  if ( blockb <= shortcut_(0,2) ) then
    sh=blockb
    do sh2=sh, shortcut_(0,2), shortcut_(0,1)
      do i=blockb2+shortcut_(sh2,2),shortcut_(sh2+1,2)-1, istep
        ii += 1
        if (ii>bs) then
          print *,  irp_here
          stop 'ii>bs'
        endif
        org_i = sort_idx_(i,2)
        do j=shortcut_(sh2,2),shortcut_(sh2+1,2)-1
          if(i == j) cycle
          org_j = sort_idx_(j,2)
          ext = popcnt(xor(sorted_(1,i,2), sorted_(1,j,2)))
          if (ext > 4) cycle
          do ni=2,N_int
            ext = ext + popcnt(xor(sorted_(ni,i,2), sorted_(ni,j,2)))
            if (ext > 4) exit
          end do
          if(ext == 4) then
            call i_h_j (dav_det(1,1,org_j),dav_det(1,1,org_i),n_int,hij)
            call get_s2(dav_det(1,1,org_j),dav_det(1,1,org_i),n_int,s2)
!            call i_h_j (sorted_(1,j,2),sorted_(1,i,2),n_int,hij)
!            call get_s2(sorted_(1,j,2),sorted_(1,i,2),n_int,s2) 
            if(.not. wrotten(ii)) then
              wrotten(ii) = .true.
              idx(ii) = org_i
              vt (:,ii) = 0d0
              st (:,ii) = 0d0
            end if
            do istate=1,N_states_diag
              vt (istate,ii) = vt (istate,ii) +hij*dav_ut(istate,org_j)
              st (istate,ii) = st (istate,ii) +s2*dav_ut(istate,org_j)
            enddo
          end if
        end do
      end do
    enddo
  endif

  N=0
  do i=1,bs
    if(wrotten(i)) then
      N += 1
      idx(N) = idx(i)
      vt(:,N) = vt(:,i)
      st(:,N) = st(:,i)
    end if
  end do


end subroutine




subroutine davidson_collect(N, idx, vt, st , v0t, s0t)
  implicit none


  integer             , intent(in)        :: N
  integer             , intent(in)        :: idx(N)
  double precision    , intent(in)        :: vt(N_states_diag, N)
  double precision    , intent(in)        :: st(N_states_diag, N)
  double precision    , intent(inout)     :: v0t(N_states_diag,dav_size)
  double precision    , intent(inout)     :: s0t(N_states_diag,dav_size)

  integer :: i, j, k

  do i=1,N
    k = idx(i)
    do j=1,N_states_diag
      v0t(j,k) = v0t(j,k) + vt(j,i)
      s0t(j,k) = s0t(j,k) + st(j,i)
    enddo
  end do
end subroutine


subroutine davidson_init(zmq_to_qp_run_socket,u,n0,n,n_st,update_dets)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR), intent(out)  :: zmq_to_qp_run_socket
  integer, intent(in) :: n0,n, n_st, update_dets
  double precision, intent(in) :: u(n0,n_st)
  integer :: i,k

  
  if (update_dets == 1) then
    dav_size = n
    touch dav_size
    do i=1,dav_size
      do k=1,N_int
        dav_det(k,1,i) = psi_det(k,1,i)
        dav_det(k,2,i) = psi_det(k,2,i)
      enddo
    enddo
    touch dav_det
  endif

  do i=1,n
    do k=1,n_st
      dav_ut(k,i) = u(i,k)
    enddo
  enddo

  soft_touch dav_ut

  call new_parallel_job(zmq_to_qp_run_socket,"davidson")
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

  integer                        :: worker_id, task_id, blockb
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
  

  integer                  :: blockb, blockb2, istep
  integer                  :: N
  integer             ,  allocatable      :: idx(:)
  double precision    ,  allocatable      :: vt(:,:)
  double precision    ,  allocatable      :: st(:,:)
  
  integer :: bs, i, j
  
  allocate(idx(1), vt(1,1), st(1,1))

  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, task)
    if(task_id == 0) exit
    read (task,*) blockb, blockb2, istep
    bs = shortcut_(blockb+1,1) - shortcut_(blockb, 1)
    do i=blockb, shortcut_(0,2), shortcut_(0,1)
    do j=i, min(i, shortcut_(0,2))
      bs += shortcut_(j+1,2) - shortcut_(j, 2)
    end do
    end do
    if(bs > size(idx)) then
      deallocate(idx, vt, st)
      allocate(idx(bs)) 
      allocate(vt(N_states_diag, bs)) 
      allocate(st(N_states_diag, bs))
    end if
  
    call davidson_process(blockb, blockb2, N, idx, vt, st, bs, istep)
    call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id)
    call davidson_push_results(zmq_socket_push, blockb, blockb2, N, idx, vt, st, task_id)
  end do
  deallocate(idx, vt, st)

end subroutine



subroutine davidson_push_results(zmq_socket_push, blockb, blocke, N, idx, vt, st, task_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)    :: zmq_socket_push
  integer             ,intent(in)    :: task_id

  integer             ,intent(in)    :: blockb, blocke
  integer             ,intent(in)    :: N
  integer             ,intent(in)    :: idx(N)
  double precision    ,intent(in)    :: vt(N_states_diag, N)
  double precision    ,intent(in)    :: st(N_states_diag, N)
  integer                            :: rc

  rc = f77_zmq_send( zmq_socket_push, blockb, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "davidson_push_results failed to push blockb"

  rc = f77_zmq_send( zmq_socket_push, blocke, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "davidson_push_results failed to push blocke"
  
  rc = f77_zmq_send( zmq_socket_push, N, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "davidson_push_results failed to push N"

  rc = f77_zmq_send( zmq_socket_push, idx, 4*N, ZMQ_SNDMORE)
  if(rc /= 4*N) stop "davidson_push_results failed to push idx"

  rc = f77_zmq_send( zmq_socket_push, vt, 8*N_states_diag* N, ZMQ_SNDMORE)
  if(rc /= 8*N_states_diag* N) stop "davidson_push_results failed to push vt"

  rc = f77_zmq_send( zmq_socket_push, st, 8*N_states_diag* N, ZMQ_SNDMORE)
  if(rc /= 8*N_states_diag* N) stop "davidson_push_results failed to push st"

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



subroutine davidson_pull_results(zmq_socket_pull, blockb, blocke, N, idx, vt, st, task_id)
  use f77_zmq
  implicit none
  
  integer(ZMQ_PTR)    ,intent(in)     :: zmq_socket_pull
  integer             ,intent(out)    :: task_id
  integer             ,intent(out)    :: blockb, blocke
  integer             ,intent(out)    :: N
  integer             ,intent(out)    :: idx(*)
  double precision    ,intent(out)    :: vt(N_states_diag, *)
  double precision    ,intent(out)    :: st(N_states_diag, *)

  integer                            :: rc

  rc = f77_zmq_recv( zmq_socket_pull, blockb, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to pull blockb"
  
  rc = f77_zmq_recv( zmq_socket_pull, blocke, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to pull blocke"
  
  rc = f77_zmq_recv( zmq_socket_pull, N, 4, 0)
  if(rc /= 4) stop "davidson_push_results failed to pull N"
  
  rc = f77_zmq_recv( zmq_socket_pull, idx, 4*N, 0)
  if(rc /= 4*N) stop "davidson_push_results failed to pull idx"

  rc = f77_zmq_recv( zmq_socket_pull, vt, 8*N_states_diag* N, 0)
  if(rc /= 8*N_states_diag* N) stop "davidson_push_results failed to pull vt"

  rc = f77_zmq_recv( zmq_socket_pull, st, 8*N_states_diag* N, 0)
  if(rc /= 8*N_states_diag* N) stop "davidson_push_results failed to pull st"

  rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)
  if(rc /= 4) stop "davidson_pull_results failed to pull task_id"

! Activate if zmq_socket_pull is a REP
  rc = f77_zmq_send( zmq_socket_pull, 0, 4, 0)
  if (rc /= 4) then
    print *,  irp_here, ' : f77_zmq_send (zmq_socket_pull,...'
    stop 'error'
  endif

end subroutine



subroutine davidson_collector(zmq_to_qp_run_socket, zmq_socket_pull , v0, s0, LDA)
  use f77_zmq
  implicit none

  integer                        :: LDA
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  
  double precision    ,intent(inout) :: v0(LDA, N_states_diag)
  double precision    ,intent(inout) :: s0(LDA, N_states_diag)
  
  integer                          :: more, task_id, taskn
  
  integer                        :: blockb, blocke
  integer                        :: N
  integer             , allocatable :: idx(:)
  double precision    , allocatable :: vt(:,:), v0t(:,:), s0t(:,:)
  double precision    , allocatable :: st(:,:)
  
  integer :: msize

  msize = (1 + max_blocksize)*2
  allocate(idx(msize)) 
  allocate(vt(N_states_diag, msize)) 
  allocate(st(N_states_diag, msize)) 
  allocate(v0t(N_states_diag, dav_size)) 
  allocate(s0t(N_states_diag, dav_size)) 
  
  v0t = 0.d0
  s0t = 0.d0

  more = 1
  
  do while (more == 1)
    call davidson_pull_results(zmq_socket_pull, blockb, blocke, N, idx, vt, st, task_id)
    !DIR$ FORCEINLINE
    call davidson_collect(N, idx, vt, st , v0t, s0t)
    call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more)
  end do
  deallocate(idx,vt,st)

  integer :: i,j
  do j=1,N_states_diag
   do i=1,dav_size
     v0(i,j) = v0t(j,i)
     s0(i,j) = s0t(j,i)
   enddo
  enddo

  deallocate(v0t,s0t)
end subroutine


subroutine davidson_run(zmq_to_qp_run_socket , v0, s0, LDA)
  use f77_zmq
  implicit none
  
  integer                        :: LDA
  integer(ZMQ_PTR), intent(in)   :: zmq_to_qp_run_socket
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_collector
  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull
  
  integer :: i
  integer, external              :: omp_get_thread_num

  double precision    , intent(inout)     :: v0(LDA, N_states_diag)
  double precision    , intent(inout)     :: s0(LDA, N_states_diag)
  
  
  zmq_collector = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  call davidson_collector(zmq_collector, zmq_socket_pull , v0, s0, LDA)
  call end_zmq_to_qp_run_socket(zmq_collector)
  call end_zmq_pull_socket(zmq_socket_pull)
  call davidson_miniserver_end()

end subroutine



subroutine davidson_miniserver_run(update_dets)
  use f77_zmq
  implicit none
  integer                 update_dets
  integer(ZMQ_PTR)        responder
  character*(64)          address
  character(len=:), allocatable :: buffer
  integer                 rc
  
  allocate (character(len=20) :: buffer)
  address = 'tcp://*:11223'
  
  PROVIDE dav_det dav_ut dav_size

  responder = f77_zmq_socket(zmq_context, ZMQ_REP)
  rc        = f77_zmq_bind(responder,address)
  
  do
    rc = f77_zmq_recv(responder, buffer, 5, 0)
    if (buffer(1:rc) == 'end') then
      rc = f77_zmq_send (responder, "end", 3, 0)
      exit
    else if (buffer(1:rc) == 'det') then
      rc = f77_zmq_send (responder, dav_size, 4, ZMQ_SNDMORE)
      rc = f77_zmq_send (responder, dav_det, 16*N_int*dav_size, 0)
    else if (buffer(1:rc) == 'ut') then
      rc = f77_zmq_send (responder, update_dets, 4, ZMQ_SNDMORE)
      rc = f77_zmq_send (responder, dav_size, 4, ZMQ_SNDMORE)
      rc = f77_zmq_send (responder, dav_ut, 8*dav_size*N_states_diag, 0)
    endif
  enddo

  rc = f77_zmq_close(responder)
end subroutine


subroutine davidson_miniserver_end()
  implicit none
  use f77_zmq
  
  integer(ZMQ_PTR)        requester
  character*(64)          address
  integer                 rc
  character*(64)          buf
  
  address = trim(qp_run_address)//':11223'
  requester = f77_zmq_socket(zmq_context, ZMQ_REQ)
  rc        = f77_zmq_connect(requester,address)

  rc = f77_zmq_send(requester, "end", 3, 0)
  rc = f77_zmq_recv(requester, buf, 3, 0)
  rc = f77_zmq_close(requester)
end subroutine

  
subroutine davidson_miniserver_get(force_update)
  implicit none
  use f77_zmq
  logical, intent(in) ::  force_update 
  integer(ZMQ_PTR)        requester
  character*(64)          address
  character*(20)          buffer
  integer                 rc, update_dets
  
  address = trim(qp_run_address)//':11223'
  
  requester = f77_zmq_socket(zmq_context, ZMQ_REQ)
  rc        = f77_zmq_connect(requester,address)

  rc = f77_zmq_send(requester, 'ut', 2, 0)
  rc = f77_zmq_recv(requester, update_dets, 4, 0)
  rc = f77_zmq_recv(requester, dav_size, 4, 0)
  
  if (update_dets == 1 .or. force_update) then
    TOUCH dav_size
  endif
  rc = f77_zmq_recv(requester, dav_ut, 8*dav_size*N_states_diag, 0)
  SOFT_TOUCH dav_ut 
  if (update_dets == 1 .or. force_update) then
    rc = f77_zmq_send(requester, 'det', 3, 0)
    rc = f77_zmq_recv(requester, dav_size, 4, 0)
    rc = f77_zmq_recv(requester, dav_det, 16*N_int*dav_size, 0)
    SOFT_TOUCH dav_det 
  endif

end subroutine



 BEGIN_PROVIDER [ integer(bit_kind), dav_det, (N_int, 2, dav_size) ]
 use bitmasks
 implicit none
 BEGIN_DOC
! Temporary arrays for parallel davidson
!
! Touched in davidson_miniserver_get
 END_DOC
 integer :: i,k

 dav_det = 0_bit_kind
END_PROVIDER

BEGIN_PROVIDER [ double precision, dav_ut, (N_states_diag, dav_size) ]
 use bitmasks
 implicit none
 BEGIN_DOC
! Temporary arrays for parallel davidson
!
! Touched in davidson_miniserver_get
 END_DOC
 dav_ut = -huge(1.d0)
END_PROVIDER


BEGIN_PROVIDER [ integer, dav_size ]
 implicit none
 BEGIN_DOC
! Size of the arrays for Davidson
!
! Touched in davidson_miniserver_get
 END_DOC
 dav_size = 1
END_PROVIDER


 BEGIN_PROVIDER [ integer, shortcut_, (0:dav_size+1, 2) ]
&BEGIN_PROVIDER [ integer(bit_kind), version_, (N_int, dav_size, 2) ]
&BEGIN_PROVIDER [ integer(bit_kind), sorted_, (N_int, dav_size, 2) ]
&BEGIN_PROVIDER [ integer, sort_idx_, (dav_size, 2) ]
&BEGIN_PROVIDER [ integer, max_blocksize ]
implicit none
  call sort_dets_ab_v(dav_det, sorted_(1,1,1), sort_idx_(1,1), shortcut_(0,1), version_(1,1,1), dav_size, N_int)
  call sort_dets_ba_v(dav_det, sorted_(1,1,2), sort_idx_(1,2), shortcut_(0,2), version_(1,1,2), dav_size, N_int)
  max_blocksize = max(shortcut_(0,1), shortcut_(0,2))
END_PROVIDER


