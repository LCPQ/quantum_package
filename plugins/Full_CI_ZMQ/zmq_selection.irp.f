subroutine ZMQ_selection(N_in, pt2)
  use f77_zmq
  use selection_types
  
  implicit none
  
  character*(512)                :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  integer, intent(in)            :: N_in
  type(selection_buffer)         :: b
  integer                        :: i, N
  integer, external              :: omp_get_thread_num
  double precision, intent(out)  :: pt2(N_states)
  
  
  PROVIDE fragment_count

  if (.True.) then
    PROVIDE pt2_e0_denominator
    N = max(N_in,1)
    provide nproc
    call new_parallel_job(zmq_to_qp_run_socket,"selection")
    call zmq_put_psi(zmq_to_qp_run_socket,1,pt2_e0_denominator,size(pt2_e0_denominator))
    call zmq_set_running(zmq_to_qp_run_socket)
    call create_selection_buffer(N, N*2, b)
  endif

  integer :: i_generator, i_generator_start, i_generator_max, step
!  step = int(max(1.,10*elec_num/mo_tot_num)

  step = int(5000000.d0 / dble(N_int * N_states * elec_num * elec_num * mo_tot_num * mo_tot_num ))
  step = max(1,step)
  do i= 1, N_det_generators,step
    i_generator_start = i
    i_generator_max = min(i+step-1,N_det_generators)
    write(task,*) i_generator_start, i_generator_max, 1, N
    call add_task_to_taskserver(zmq_to_qp_run_socket,task)
  end do

  !$OMP PARALLEL DEFAULT(shared)  SHARED(b, pt2)  PRIVATE(i) NUM_THREADS(nproc+1)
  i = omp_get_thread_num()
  if (i==0) then
    call selection_collector(b, pt2)
  else
    call selection_slave_inproc(i)
  endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'selection')
  if (N_in > 0) then
    call fill_H_apply_buffer_no_selection(b%cur,b%det,N_int,0) !!! PAS DE ROBIN
    call copy_H_apply_buffer_to_wf()
    if (s2_eig) then
      call make_s2_eigenfunction
    endif
  endif
end subroutine


subroutine selection_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call run_selection_slave(1,i,pt2_e0_denominator)
end

subroutine selection_collector(b, pt2)
  use f77_zmq
  use selection_types
  use bitmasks
  implicit none


  type(selection_buffer), intent(inout) :: b
  double precision, intent(out)       :: pt2(N_states)
  double precision                   :: pt2_mwen(N_states)
  integer(ZMQ_PTR),external      :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket

  integer(ZMQ_PTR), external     :: new_zmq_pull_socket
  integer(ZMQ_PTR)               :: zmq_socket_pull

  integer :: msg_size, rc, more
  integer :: acc, i, j, robin, N, ntask
  double precision, allocatable :: val(:)
  integer(bit_kind), allocatable :: det(:,:,:)
  integer, allocatable :: task_id(:)
  integer :: done
  real :: time, time0
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()
  zmq_socket_pull = new_zmq_pull_socket()
  allocate(val(b%N), det(N_int, 2, b%N), task_id(N_det))
  done = 0
  more = 1
  pt2(:) = 0d0
  call CPU_TIME(time0)
  do while (more == 1)
    call pull_selection_results(zmq_socket_pull, pt2_mwen, val(1), det(1,1,1), N, task_id, ntask)
    pt2 += pt2_mwen
    do i=1, N
      call add_to_selection_buffer(b, det(1,1,i), val(i))
    end do

    do i=1, ntask
      if(task_id(i) == 0) then
          print *,  "Error in collector"
      endif
      call zmq_delete_task(zmq_to_qp_run_socket,zmq_socket_pull,task_id(i),more)
    end do
    done += ntask
    call CPU_TIME(time)
!    print *, "DONE" , done, time - time0
  end do


  call end_zmq_to_qp_run_socket(zmq_to_qp_run_socket)
  call end_zmq_pull_socket(zmq_socket_pull)
  call sort_selection_buffer(b)
end subroutine

