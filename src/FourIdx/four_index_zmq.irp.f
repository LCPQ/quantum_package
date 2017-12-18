subroutine four_index_transform_zmq(map_a,map_c,matrix_B,LDB,        &
      i_start, j_start, k_start, l_start,                            &
      i_end  , j_end  , k_end  , l_end  ,                            &
      a_start, b_start, c_start, d_start,                            &
      a_end  , b_end  , c_end  , d_end  )
  implicit none
  use f77_zmq
  use map_module
  BEGIN_DOC
! Performs a four-index transformation of map_a(N^4) into map_c(M^4) using b(NxM)
! C_{abcd} = \sum_{ijkl} A_{ijkl}.B_{ia}.B_{jb}.B_{kc}.B_{ld}
! Loops run over *_start->*_end
  END_DOC
  type(map_type), intent(in)     :: map_a
  type(map_type), intent(inout)  :: map_c
  integer, intent(in)            :: LDB
  double precision, intent(in)   :: matrix_B(LDB,*)
  integer, intent(in)            :: i_start, j_start, k_start, l_start
  integer, intent(in)            :: i_end  , j_end  , k_end  , l_end
  integer, intent(in)            :: a_start, b_start, c_start, d_start
  integer, intent(in)            :: a_end  , b_end  , c_end  , d_end

  integer                        :: i_max, j_max, k_max, l_max
  integer                        :: i_min, j_min, k_min, l_min
  integer                        :: i, j, k, l, ik, ll
  integer                        :: l_start_block, l_end_block, l_block
  integer                        :: a, b, c, d
  double precision, external     :: get_ao_bielec_integral
  integer*8                      :: ii
  integer(key_kind)              :: idx
  real(integral_kind)            :: tmp
  integer(key_kind), allocatable :: key(:)
  real(integral_kind), allocatable :: value(:)
  integer*8, allocatable         :: l_pointer(:)

  ASSERT (k_start == i_start)
  ASSERT (l_start == j_start)
  ASSERT (a_start == c_start)
  ASSERT (b_start == d_start)

  i_min = min(i_start,a_start)
  i_max = max(i_end  ,a_end  )
  j_min = min(j_start,b_start)
  j_max = max(j_end  ,b_end  )
  k_min = min(k_start,c_start)
  k_max = max(k_end  ,c_end  )
  l_min = min(l_start,d_start)
  l_max = max(l_end  ,d_end  )

  ASSERT (0 < i_max)
  ASSERT (0 < j_max)
  ASSERT (0 < k_max)
  ASSERT (0 < l_max)
  ASSERT (LDB >= i_max)
  ASSERT (LDB >= j_max)
  ASSERT (LDB >= k_max)
  ASSERT (LDB >= l_max)


  integer(ZMQ_PTR) :: zmq_to_qp_run_socket
  integer(ZMQ_PTR) :: zmq_socket_pull
  call new_parallel_job(zmq_to_qp_run_socket,zmq_socket_pull,'four_idx')

  integer*8 :: new_size
  new_size = max(2048_8, 5_8 * map_a % n_elements )

  integer :: npass
  integer*8 :: tempspace

  tempspace = (new_size * 16_8) / (2048_8 * 2048_8)
  npass = int(min(int(l_end-l_start,8),1_8 + tempspace / 2048_8),4)   ! 2 GiB of scratch space
  l_block = (l_end-l_start+1)/npass

  ! Create tasks
  ! ============

  character(len=256) :: task
  
  integer, external :: add_task_to_taskserver

  do l_start_block = l_start, l_end, l_block
    l_end_block = min(l_end, l_start_block+l_block-1)
    write(task,'(16(I10,X))')                                        &
        i_start, j_start, k_start, l_start_block,                    &
        i_end  , j_end  , k_end  , l_end_block  ,                    &
        a_start, b_start, c_start, d_start,                          &
        a_end  , b_end  , c_end  , d_end
    if (add_task_to_taskserver(zmq_to_qp_run_socket,trim(task)) == -1) then
      stop 'Unable to add task to server'
    endif
  enddo
  
  integer, external              :: zmq_set_running
  if (zmq_set_running(zmq_to_qp_run_socket) == -1) then
    print *,  irp_here, ': Failed in zmq_set_running'
  endif
  

  PROVIDE nproc

  integer :: ithread, sqnproc
  sqnproc = int(sqrt(float(nproc))+0.5)
  call omp_set_nested(.True.)
  call omp_set_dynamic(.True.)
  !$OMP PARALLEL NUM_THREADS(1+max(1,sqnproc)) PRIVATE(ithread)
  ithread = omp_get_thread_num()
  if (ithread==0) then
    call four_idx_collector(zmq_socket_pull,map_c)
  else
    call four_index_transform_slave_inproc(ithread)
  endif
  !$OMP END PARALLEL

  call end_parallel_job(zmq_to_qp_run_socket, zmq_socket_pull, 'four_idx')


end


subroutine four_index_transform_slave_tcp(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
! Computes a buffer of integrals. i is the ID of the current thread.
  END_DOC
  call four_index_transform_slave(0,i)
end


subroutine four_index_transform_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i
  BEGIN_DOC
! Computes a buffer of integrals. i is the ID of the current thread.
  END_DOC
  call four_index_transform_slave(1,i)
end



subroutine four_index_transform_slave(thread,worker_id)
  use f77_zmq
  implicit none

  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket
  integer,intent(in)             :: worker_id, thread
  integer                        :: task_id
  character*(512)                :: msg

  integer                        :: i_start, j_start, k_start, l_start_block
  integer                        :: i_end  , j_end  , k_end  , l_end_block
  integer                        :: a_start, b_start, c_start, d_start
  integer                        :: a_end  , b_end  , c_end  , d_end

  integer, external              :: get_task_from_taskserver
  integer(ZMQ_PTR), external     :: new_zmq_to_qp_run_socket

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()

  integer, external :: connect_to_taskserver
  if (connect_to_taskserver(zmq_to_qp_run_socket,worker_id,thread) == -1) then
    call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
    return
  endif

  do
    if (get_task_from_taskserver(zmq_to_qp_run_socket,worker_id, task_id, msg) == -1) then
      exit
    endif
    if(task_id == 0) exit
    read (msg,*) &
          i_start, j_start, k_start, l_start_block,                      &
          i_end  , j_end  , k_end  , l_end_block  ,                      &
          a_start, b_start, c_start, d_start,                            &
          a_end  , b_end  , c_end  , d_end

    call four_index_transform_slave_work(ao_integrals_map,           &
        mo_coef, size(mo_coef,1),                                    &
        i_start, j_start, k_start, l_start_block,                    &
        i_end  , j_end  , k_end  , l_end_block  ,                    &
        a_start, b_start, c_start, d_start,                          &
        a_end  , b_end  , c_end  , d_end,                            &
        task_id, worker_id, thread)


  enddo
  integer, external :: disconnect_from_taskserver
  if (disconnect_from_taskserver(zmq_to_qp_run_socket,thread) == -1) then
    continue
  endif
  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)

end


BEGIN_PROVIDER [ integer, nthreads_four_idx ]
 implicit none
 BEGIN_DOC
 ! Number of threads for 4-index transformation
 END_DOC
 nthreads_four_idx = nproc
 character*(32) :: env
 call getenv('NTHREADS_FOUR_IDX',env)
 if (trim(env) /= '') then
   read(env,*) nthreads_four_idx
 endif
 call write_int(6,nthreads_four_idx,'Number of threads for 4-index transformation')
END_PROVIDER



subroutine four_idx_collector(zmq_socket_pull,map_c)
  use f77_zmq
  use map_module
  implicit none
  type(map_type), intent(inout)  :: map_c
  integer(ZMQ_PTR), intent(in)   :: zmq_socket_pull

  integer :: more
  integer, external              :: zmq_delete_task
  integer(ZMQ_PTR), external     :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket
  integer                        :: task_id


  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()

  more = 1
  do while (more == 1)
    call four_idx_pull_results(zmq_socket_pull, map_c, task_id)
    if (task_id > 0) then
      if (zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id,more) == -1) then
        stop 'Unable to delete task'
      endif
    endif
  enddo

  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)

end


subroutine four_idx_pull_results(zmq_socket_pull, map_c, task_id)
  use f77_zmq
  use map_module
  implicit none
  type(map_type), intent(inout)   :: map_c
  integer(ZMQ_PTR), intent(in) :: zmq_socket_pull

  integer, intent(out) :: task_id

  integer                            :: rc, sze
  integer*8                          :: rc8


  rc = f77_zmq_recv( zmq_socket_pull, task_id, 4, 0)
  if(rc /= 4) stop 'four_idx_pull_results failed to pull task_id'

  if (task_id > 0) then
    IRP_IF ZMQ_PUSH
    IRP_ELSE
      rc = f77_zmq_send( zmq_socket_pull, 'ok', 2, 0)
      if (rc /= 2) then
        print *,  irp_here, ' : f77_zmq_send (zmq_socket_pull,...'
        stop 'error'
      endif
    IRP_ENDIF
    call map_merge(map_c)
    return
  endif

  rc = f77_zmq_recv( zmq_socket_pull, sze, 4, 0)
  if(rc /= 4) stop 'four_idx_pull_results failed to pull sze'

  integer(key_kind), allocatable :: key(:)
  real(integral_kind), allocatable :: value(:)

  allocate(key(sze), value(sze))

  rc8 = f77_zmq_recv8( zmq_socket_pull, key, int(key_kind*sze,8), 0)
  if(rc8 /= key_kind*sze) stop 'four_idx_pull_results failed to pull key'

  rc8 = f77_zmq_recv8( zmq_socket_pull, value, int(integral_kind*sze,8), 0)
  if(rc8 /= integral_kind*sze) stop 'four_idx_pull_results failed to pull value'

! Activate if zmq_socket_pull is a REP
  IRP_IF ZMQ_PUSH
  IRP_ELSE
    rc = f77_zmq_send( zmq_socket_pull, 'ok', 2, 0)
    if (rc /= 2) then
      print *,  irp_here, ' : f77_zmq_send (zmq_socket_pull,...'
      stop 'error'
    endif
  IRP_ENDIF

  call map_update(map_c, key, value, sze, mo_integrals_threshold)  

  deallocate(key, value)
end



subroutine four_idx_push_results(zmq_socket_push, key, value, sze, task_id)
  use f77_zmq
  use map_module
  implicit none
  integer, intent(in)             :: sze
  integer(key_kind), intent(in)   :: key(sze)
  real(integral_kind), intent(in) :: value(sze)
  integer(ZMQ_PTR), intent(in)    :: zmq_socket_push
  integer, intent(in)             :: task_id

  integer                         :: rc
  integer*8                       :: rc8


  if (task_id > 0) then
    rc = f77_zmq_send( zmq_socket_push, task_id, 4, 0)
    if(rc /= 4) stop 'four_idx_push_results failed to push task_id'
  else
    rc = f77_zmq_send( zmq_socket_push, task_id, 4, ZMQ_SNDMORE)
    if(rc /= 4) stop 'four_idx_push_results failed to push task_id'

    rc = f77_zmq_send( zmq_socket_push, sze, 4, ZMQ_SNDMORE)
    if(rc /= 4) stop 'four_idx_push_results failed to push sze'

    rc8 = f77_zmq_send8( zmq_socket_push, key, int(key_kind*sze,8), ZMQ_SNDMORE)
    if(rc8 /= key_kind*sze) then
      print *,  sze, key_kind, rc8
      stop 'four_idx_push_results failed to push key'
    endif

    rc8 = f77_zmq_send8( zmq_socket_push, value, int(integral_kind*sze,8), 0)
    if(rc8 /= integral_kind*sze) then
      print *,  sze, integral_kind, rc8
      stop 'four_idx_push_results failed to push value'
    endif
  endif


! Activate if zmq_socket_push is a REP
IRP_IF ZMQ_PUSH
IRP_ELSE
  character*(2) :: reply
  rc = f77_zmq_recv( zmq_socket_push, reply, 2, 0)
  if (reply(1:2) /= 'ok') then
    print *,  reply(1:rc)
    print *,  irp_here, ' : f77_zmq_send (zmq_socket_push,...'
    stop 'error'
  endif
IRP_ENDIF

end


