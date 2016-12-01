program fci_zmq
  implicit none
  integer                        :: i,j,k
  logical, external              :: detEq
  
  double precision, allocatable  :: pt2(:)
  integer                        :: degree
  integer                        :: n_det_before, to_select
  double precision               :: threshold_davidson_in
  
  allocate (pt2(N_states))
  
  pt2 = 1.d0
  threshold_davidson_in = threshold_davidson
  threshold_davidson = 1.d-5
  SOFT_TOUCH threshold_davidson
  
  if (N_det > N_det_max) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = N_det_max
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    do k=1,N_states
      print*,'State ',k
      print *,  'PT2      = ', pt2(k)
      print *,  'E        = ', CI_energy(k)
      print *,  'E+PT2    = ', CI_energy(k) + pt2(k)
      print *,  '-----'
    enddo
  endif
  double precision               :: E_CI_before(N_states)
  
  
  print*,'Beginning the selection ...'
  E_CI_before(1:N_states) = CI_energy(1:N_states)
  n_det_before = 0
  
  do while ( (N_det < N_det_max) .and. (maxval(abs(pt2(1:N_states))) > pt2_max) )
    
    print *,  'N_det          = ', N_det
    print *,  'N_states       = ', N_states
    do k=1, N_states
      print*,'State ',k
      print *,  'PT2            = ', pt2(k)
      print *,  'E              = ', CI_energy(k)
      print *,  'E(before)+PT2  = ', E_CI_before(k)+pt2(k)
    enddo
    print *,  '-----'
    if(N_states.gt.1)then
      print*,'Variational Energy difference'
      do i = 2, N_states
        print*,'Delta E = ',CI_energy(i) - CI_energy(1)
      enddo
    endif
    if(N_states.gt.1)then
      print*,'Variational + perturbative Energy difference'
      do i = 2, N_states
        print*,'Delta E = ',E_CI_before(i)+ pt2(i) - (E_CI_before(1) + pt2(1))
      enddo
    endif
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))

    n_det_before = N_det
    to_select = 3*N_det
    to_select = max(1024-to_select, to_select)
    to_select = min(to_select, N_det_max-n_det_before)
    call ZMQ_selection(to_select, pt2)
    
    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det == N_det_max) then
      threshold_davidson = threshold_davidson_in
      SOFT_TOUCH threshold_davidson
    endif
    call diagonalize_CI
    call save_wavefunction
    call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  enddo

  if (N_det < N_det_max) then
      threshold_davidson = threshold_davidson_in
      SOFT_TOUCH threshold_davidson
      call diagonalize_CI
      call save_wavefunction
      call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  endif

  if(do_pt2_end)then
    print*,'Last iteration only to compute the PT2'
    threshold_selectors = max(threshold_selectors,threshold_selectors_pt2)
    threshold_generators = max(threshold_generators,threshold_generators_pt2)
    TOUCH threshold_selectors threshold_generators
    E_CI_before(1:N_states) = CI_energy(1:N_states)
    call ZMQ_selection(0, pt2)
    print *,  'Final step'
    print *,  'N_det    = ', N_det
    print *,  'N_states = ', N_states
    do k=1,N_states
      print *, 'State', k
      print *,  'PT2      = ', pt2
      print *,  'E        = ', E_CI_before
      print *,  'E+PT2    = ', E_CI_before+pt2
      print *,  '-----'
    enddo
    call ezfio_set_full_ci_zmq_energy_pt2(E_CI_before(1)+pt2(1))
  endif
  call save_wavefunction
  call ezfio_set_full_ci_zmq_energy(CI_energy(1))
  call ezfio_set_full_ci_zmq_energy_pt2(E_CI_before(1)+pt2(1))
end




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

