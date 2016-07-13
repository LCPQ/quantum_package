
program Full_CI_ZMQ
  use f77_zmq
  implicit none
  BEGIN_DOC
! Massively parallel Full-CI
  END_DOC

  integer :: i,ithread

  integer(ZMQ_PTR) :: zmq_socket_push
  integer(ZMQ_PTR), external :: new_zmq_push_socket
  zmq_context = f77_zmq_ctx_new ()
  PROVIDE H_apply_buffer_allocated
  
!   do while (N_det < N_det_max)

    PROVIDE ci_electronic_energy
    PROVIDE nproc
    !$OMP PARALLEL PRIVATE(i,ithread,zmq_socket_push) num_threads(nproc+1)
    ithread = omp_get_thread_num()
    if (ithread == 0) then
      call receive_selected_determinants()
    else
      zmq_socket_push = new_zmq_push_socket()
      
      do i=ithread,N_det_generators,nproc
        print *,  i , "/", N_det_generators
        call select_connected(i, 1.d-7, ci_electronic_energy,zmq_socket_push)
      enddo
      print *, "END .... "
      
      if (ithread == 1) then
        integer :: rc
        rc = f77_zmq_send(zmq_socket_push,0,1,0)
        if (rc /= 1) then
          stop 'Error sending termination signal'
        endif
      endif
      call end_zmq_push_socket(zmq_socket_push, 0)  
    endif
    !$OMP END PARALLEL
    call copy_H_apply_buffer_to_wf()
    call diagonalize_CI
    call save_wavefunction()
!   end do    

end
