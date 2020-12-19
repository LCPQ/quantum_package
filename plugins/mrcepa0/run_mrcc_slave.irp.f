
BEGIN_PROVIDER [ integer, fragment_count ]
  implicit none
  BEGIN_DOC
  ! Number of fragments for the deterministic part
  END_DOC
  fragment_count = 1 !! (elec_alpha_num-n_core_orb)**2
END_PROVIDER


subroutine run_mrcc_slave(thread,iproc,energy)
  use f77_zmq
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

  logical :: done

  double precision,allocatable :: mrcc_detail(:,:)
  integer :: ind(1)
  integer :: Nindex
  
  integer(bit_kind),allocatable :: abuf(:,:,:)
  integer(bit_kind) :: mask(N_int,2), omask(N_int,2)
 
  double precision,allocatable :: delta_ij_loc(:,:,:) 
  !double precision,allocatable :: delta_ij_s2_loc(:,:,:)
  integer :: h,p,n
  logical :: ok
  double precision :: contrib(N_states)

  allocate(delta_ij_loc(N_states,N_det_non_ref,2) )
  !,delta_ij_s2_loc(N_states,N_det_non_ref,N_det_ref) &


  allocate(abuf(N_int, 2, N_det_non_ref))

  allocate(mrcc_detail(N_states, N_det_generators))
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_push      = new_zmq_push_socket(thread)
  call connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread)
  if(worker_id == -1) then
    print *, "WORKER -1"
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    call end_zmq_push_socket(zmq_socket_push,thread)
    return
  end if
  !buf%N = 0
  ctask = 1
  Nindex=1
  mrcc_detail = 0d0
  do
    call get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id(ctask), task)

    done = task_id(ctask) == 0
    if (done) then
      ctask = ctask - 1
    else
      integer :: i_generator, i_i_generator, subset
      read (task,*) subset, ind
!      if(buf%N == 0) then
!        ! Only first time 
!        call create_selection_buffer(1, 2, buf)
!      end if
      do i_i_generator=1, Nindex
        contrib = 0d0
        i_generator = ind(i_i_generator)
        delta_ij_loc = 0d0
        !delta_ij_s2_loc = 0d0
        !call select_connected(i_generator,energy,mrcc_detail(1, i_i_generator),buf,subset)

!!!!!!!!!!!!!!!!!!!!!!
      !if(mod(gen, 1000) == 0) print *, "mrcc ", gen, "/", N_det_generators
        do h=1, hh_shortcut(0)
          call apply_hole_local(psi_det_generators(1,1,i_generator), hh_exists(1, h), mask, ok, N_int)
          if(.not. ok) cycle
          omask = 0_bit_kind
          if(hh_exists(1, h) /= 0) omask = mask
          n = 1
          do p=hh_shortcut(h), hh_shortcut(h+1)-1
            call apply_particle_local(mask, pp_exists(1, p), abuf(1,1,n), ok, N_int)
            if(ok) n = n + 1
            if(n > N_det_non_ref) stop "Buffer too small in MRCC..."
          end do
          n = n - 1

          if(n /= 0) then
            call mrcc_part_dress(delta_ij_loc(1,1,1), delta_ij_loc(1,1,2), &
                  i_generator,n,abuf,N_int,omask,contrib)
          endif
        end do
!!!!!!!!!!!!!!!!!!!!!!
        !print *, "contrib", i_generator, contrib
        mrcc_detail(:, i_i_generator) = contrib
        if(Nindex /= 1) stop "run_mrcc_slave multiple dress not supported"
      enddo
    endif

    if(done .or. (ctask == size(task_id)) ) then
!      if(buf%N == 0 .and. ctask > 0) stop "uninitialized selection_buffer"
      do i=1, ctask
         call task_done_to_taskserver(zmq_to_qp_run_socket,worker_id,task_id(i))
      end do
      if(ctask > 0) then
        call push_mrcc_results(zmq_socket_push, Nindex, ind, mrcc_detail, delta_ij_loc(1,1,1), task_id(1), ctask)
        mrcc_detail(:,:Nindex) = 0d0
!        buf%cur = 0
      end if
      ctask = 0
    end if

    if(done) exit
    ctask = ctask + 1
  end do
  call disconnect_from_taskserver(zmq_to_qp_run_socket,worker_id)
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_push_socket(zmq_socket_push,thread)
!  call delete_selection_buffer(buf)
end subroutine


subroutine push_mrcc_results(zmq_socket_push, N, ind, mrcc_detail, delta_loc, task_id, ntask)
  use f77_zmq
  implicit none

  integer(ZMQ_PTR), intent(in)   :: zmq_socket_push
  double precision, intent(in)   :: mrcc_detail(N_states, N_det_generators)
  double precision, intent(in)   :: delta_loc(N_states, N_det_non_ref, 2)
  integer, intent(in) :: ntask, N, ind(*), task_id(*)
  integer :: rc, i
  
  if(N/=1) stop "mrcc_stoch, tried to push N>1"

  rc = f77_zmq_send( zmq_socket_push, N, 4, ZMQ_SNDMORE)
  if(rc /= 4) stop "push"

  rc = f77_zmq_send( zmq_socket_push, ind, 4*N, ZMQ_SNDMORE)
  if(rc /= 4*N) stop "push"


  rc = f77_zmq_send( zmq_socket_push, mrcc_detail, 8*N_states*N, ZMQ_SNDMORE)
  if(rc /= 8*N_states*N) stop "push"

  rc = f77_zmq_send( zmq_socket_push, delta_loc(1,1,1), 8*N_states*N_det_non_ref*N, ZMQ_SNDMORE)
  if(rc /= 8*N_states*N_det_non_ref*N) stop "push"
  
  rc = f77_zmq_send( zmq_socket_push, delta_loc(1,1,2), 8*N_states*N_det_non_ref*N, ZMQ_SNDMORE)
  if(rc /= 8*N_states*N_det_non_ref*N) stop "push"


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


subroutine pull_mrcc_results(zmq_socket_pull, N, ind, mrcc_detail, delta_loc, task_id, ntask)
  use f77_zmq
  implicit none
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull
  double precision, intent(inout) :: mrcc_detail(N_states)
  double precision, intent(inout) :: delta_loc(N_states, N_det_non_ref, 2)
  double precision, allocatable :: mrcc_dress_mwen(:,:)
  integer, intent(out) :: ind(*)
  integer, intent(out) :: N, ntask, task_id(*)
  integer :: rc, rn, i

  allocate(mrcc_dress_mwen(N_states, N_det_non_ref))
  
  rc = f77_zmq_recv( zmq_socket_pull, N, 4, 0)
  if(rc /= 4) stop "pull"
  if(N /= 1) stop "mrcc : pull with N>1 not supported"

  rc = f77_zmq_recv( zmq_socket_pull, ind, 4, 0)
  if(rc /= 4*N) stop "pull"
  
  rc = f77_zmq_recv( zmq_socket_pull, mrcc_detail, N_states*8*N, 0)
  if(rc /= 8*N_states*N) stop "pull"
  
  !do i=1,N
    rc = f77_zmq_recv( zmq_socket_pull, delta_loc(1,1,1), N_states*8*N_det_non_ref, 0)
    if(rc /= 8*N_states*N_det_non_ref) stop "pull"
    !call add_to_dress_buffer(mrcc_dress(1), ind(i), mrcc_dress_mwen)
  !end do
  
  !do i=1,N
    rc = f77_zmq_recv( zmq_socket_pull, delta_loc(1,1,2), N_states*8*N_det_non_ref, 0)
    if(rc /= 8*N_states*N_det_non_ref) stop "pull"
    !call add_to_dress_buffer(mrcc_dress(2), ind(i), mrcc_dress_mwen)
  !end do

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
 
 
BEGIN_PROVIDER [ double precision, mrcc_workload, (N_det_generators) ]
  integer :: i
  do i=1,N_det_generators
    mrcc_workload(i) = dfloat(N_det_generators - i + 1)**2
  end do
  mrcc_workload = mrcc_workload / sum(mrcc_workload)
END_PROVIDER
            
