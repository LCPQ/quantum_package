program selection_slave
  implicit none
  BEGIN_DOC
! Helper program to compute the PT2 in distributed mode.
  END_DOC

  read_wf = .False.
  SOFT_TOUCH read_wf
  call provide_everything
  call switch_qp_run_to_master
  call run_wf
end

subroutine provide_everything
  PROVIDE H_apply_buffer_allocated mo_bielec_integrals_in_map psi_det_generators psi_coef_generators psi_det_sorted_bit psi_selectors n_det_generators n_states generators_bitmask zmq_context mo_mono_elec_integral
!   PROVIDE pt2_e0_denominator mo_tot_num N_int
end

subroutine run_wf
  use f77_zmq
  implicit none

  integer(ZMQ_PTR), external :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR) :: zmq_to_qp_run_socket
  double precision :: energy(N_states_diag)
  character*(64) :: states(2)
  integer :: rc, i
  
  call provide_everything
  
  zmq_context = f77_zmq_ctx_new ()
  states(1) = 'selection'
  states(2) = 'davidson'

  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()

  do

    call wait_for_states(states,zmq_state,2)

    if(trim(zmq_state) == 'Stopped') then

      exit

    else if (trim(zmq_state) == 'selection') then

      ! Selection
      ! ---------

      print *,  'Selection'
      call zmq_get_psi(zmq_to_qp_run_socket,1,energy,N_states_diag)
  
      !$OMP PARALLEL PRIVATE(i)
      i = omp_get_thread_num()
      call selection_slave_tcp(i, energy)
      !$OMP END PARALLEL
      print *,  'Selection done'

    else if (trim(zmq_state) == 'davidson') then

      ! Davidson
      ! --------

      print *,  'Davidson'
      call davidson_miniserver_get()
      !$OMP PARALLEL PRIVATE(i)
      i = omp_get_thread_num()
      call davidson_slave_tcp(i)
      !$OMP END PARALLEL
      print *,  'Davidson done'

    endif

  end do
end

subroutine update_energy(energy)
  implicit none
  double precision, intent(in) :: energy(N_states_diag)
  BEGIN_DOC
! Update energy when it is received from ZMQ
  END_DOC
  integer :: j,k
  do j=1,N_states
    do k=1,N_det
      CI_eigenvectors(k,j) = psi_coef(k,j)
    enddo
  enddo
  call u_0_S2_u_0(CI_eigenvectors_s2,CI_eigenvectors,N_det,psi_det,N_int)
  if (.True.) then
    do k=1,size(ci_electronic_energy)
      ci_electronic_energy(k) = energy(k)
    enddo
    TOUCH ci_electronic_energy CI_eigenvectors_s2 CI_eigenvectors
  endif

  call write_double(6,ci_energy,'Energy')
end

subroutine selection_slave_tcp(i,energy)
  implicit none
  double precision, intent(in) :: energy(N_states_diag)
  integer, intent(in)            :: i

  call run_selection_slave(0,i,energy)
end

