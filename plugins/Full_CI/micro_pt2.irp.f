program micro_pt2
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
  PROVIDE H_apply_buffer_allocated mo_bielec_integrals_in_map psi_det_generators psi_coef_generators psi_det_sorted_bit psi_selectors n_det_generators n_states generators_bitmask zmq_context
end

subroutine run_wf
  use f77_zmq
  implicit none

  integer(ZMQ_PTR), external :: new_zmq_to_qp_run_socket
  integer(ZMQ_PTR) :: zmq_to_qp_run_socket

  print *,  'Getting wave function'
  zmq_to_qp_run_socket = new_zmq_to_qp_run_socket()

  call zmq_get_psi(zmq_to_qp_run_socket, 1)
  call write_double(6,ci_energy,'Energy')
  zmq_state = 'h_apply_fci_pt2'

  call provide_everything
  integer :: rc, i

  !$OMP PARALLEL PRIVATE(i)
  i = omp_get_thread_num()
  call H_apply_FCI_PT2_slave_tcp(i)
  !$OMP END PARALLEL


end
