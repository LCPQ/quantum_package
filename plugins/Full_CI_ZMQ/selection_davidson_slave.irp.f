program selection_slave
  implicit none
  BEGIN_DOC
! Helper program to compute the PT2 in distributed mode.
  END_DOC

  read_wf = .False.
  distributed_davidson = .False.
  SOFT_TOUCH read_wf distributed_davidson
  call provide_everything
  call switch_qp_run_to_master
  call run_wf
end

subroutine provide_everything
  PROVIDE H_apply_buffer_allocated mo_bielec_integrals_in_map psi_det_generators psi_coef_generators psi_det_sorted_bit psi_selectors n_det_generators n_states generators_bitmask zmq_context n_states_diag
  PROVIDE pt2_e0_denominator mo_tot_num N_int fragment_count ci_energy mpi_master zmq_state zmq_context
  PROVIDE psi_det psi_coef
end

subroutine run_wf
  use f77_zmq
  
  implicit none
  IRP_IF MPI
    include 'mpif.h'
  IRP_ENDIF

  integer(ZMQ_PTR), external :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR) :: zmq_to_qp_run_socket
  double precision :: energy(N_states)
  character*(64) :: states(3)
  integer :: rc, i, ierr
  double precision :: t0, t1
  
  integer, external              :: zmq_get_dvector, zmq_get_N_det_generators 
  integer, external              :: zmq_get_psi, zmq_get_N_det_selectors
  integer, external              :: zmq_get_N_states_diag

  call provide_everything
  
  zmq_context = f77_zmq_ctx_new ()
  states(1) = 'selection'
  states(2) = 'davidson'
  states(3) = 'pt2'

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()

  do

    call wait_for_states(states,zmq_state,size(states))
    print *,  trim(zmq_state)

    if(zmq_state(1:7) == 'Stopped') then

      exit

    else if (zmq_state(1:9) == 'selection') then

      ! Selection
      ! ---------

      call wall_time(t0)
      if (zmq_get_psi(zmq_to_qp_run_socket,1) == -1) cycle
      if (zmq_get_N_det_generators (zmq_to_qp_run_socket, 1) == -1) cycle
      if (zmq_get_N_det_selectors(zmq_to_qp_run_socket, 1) == -1) cycle
      if (zmq_get_dvector(zmq_to_qp_run_socket,1,'energy',energy,N_states) == -1) cycle

      call wall_time(t1)
      call write_double(6,(t1-t0),'Broadcast time')
  
      !$OMP PARALLEL PRIVATE(i)
      i = omp_get_thread_num()
      call run_selection_slave(0,i,energy)
      !$OMP END PARALLEL
      print *,  'Selection done'

    else if (zmq_state(1:8) == 'davidson') then

      ! Davidson
      ! --------

      print *,  'Davidson'
      call wall_time(t0)
      if (zmq_get_psi(zmq_to_qp_run_socket,1) == -1) cycle
      if (zmq_get_N_states_diag(zmq_to_qp_run_socket,1) == -1) cycle
      if (zmq_get_dvector(zmq_to_qp_run_socket,1,'energy',energy,N_states_diag) == -1) cycle

      call wall_time(t1)
      call write_double(6,(t1-t0),'Broadcast time')

      call omp_set_nested(.True.)
      call davidson_slave_tcp(0)
      call omp_set_nested(.False.)
      print *,  'Davidson done'

    else if (zmq_state(1:3) == 'pt2') then

      ! PT2
      ! ---

      print *,  'PT2'
      call wall_time(t0)
      if (zmq_get_psi(zmq_to_qp_run_socket,1) == -1) cycle
      if (zmq_get_dvector(zmq_to_qp_run_socket,1,'energy',energy,N_states) == -1) cycle
      if (zmq_get_N_det_generators (zmq_to_qp_run_socket, 1) == -1) cycle
      if (zmq_get_N_det_selectors(zmq_to_qp_run_socket, 1) == -1) cycle

      call wall_time(t1)
      call write_double(6,(t1-t0),'Broadcast time')

      logical :: lstop
      lstop = .False.
      !$OMP PARALLEL PRIVATE(i)
      i = omp_get_thread_num()
      call run_pt2_slave(0,i,energy,lstop)
      !$OMP END PARALLEL
      print *,  'PT2 done'

    endif

    IRP_IF MPI
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      if (ierr /= MPI_SUCCESS) then
        print *,  irp_here, 'error in barrier'
      endif
    IRP_ENDIF

  end do
  IRP_IF MPI
    call MPI_finalize(i)
  IRP_ENDIF
end



