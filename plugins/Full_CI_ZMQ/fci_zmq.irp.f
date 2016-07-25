

program fci_zmq
  implicit none
  integer                        :: i,k
  logical, external :: detEq
  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  integer :: it, mit(0:6)
  mit = (/1, 246, 1600, 17528, 112067, 519459, 2685970/)
  it = 0
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  
  pt2 = 1.d0
  diag_algorithm = "Lapack"
  
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
    print *,  'PT2      = ', pt2
    print *,  'E        = ', CI_energy
    print *,  'E+PT2    = ', CI_energy+pt2
    print *,  '-----'
  endif
  double precision :: i_H_psi_array(N_states),diag_H_mat_elem,h,i_O1_psi_array(N_states)
  double precision :: E_CI_before(N_states)
  

  integer :: n_det_before
  print*,'Beginning the selection ...'
  E_CI_before = CI_energy
  do while (N_det < N_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    n_det_before = N_det
    ! call H_apply_FCI(pt2, norm_pert, H_pert_diag,  N_st)
    it += 1
    if(it > 6) stop
    call ZMQ_selection(mit(it) - mit(it-1), pt2) ! max(1000-N_det, N_det), pt2)
    
    !do i=1, N_det
      !if(popcnt(psi_det(1,1,i)) + popcnt(psi_det(2,1,i)) /= 23) stop "ZZ1" -2099.2504682049275
      !if(popcnt(psi_det(1,2,i)) + popcnt(psi_det(2,2,i)) /= 23) stop "ZZ2"
    !  do k=1,i-1
    !    if(detEq(psi_det(1,1,i), psi_det(1,1,k), N_int)) stop "ATRRGRZER"
    !  end do
    !end do
    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det > N_det_max) then
       psi_det = psi_det_sorted
       psi_coef = psi_coef_sorted
       N_det = N_det_max
       soft_touch N_det psi_det psi_coef
    endif
    call diagonalize_CI
    call save_wavefunction

    print *,  'N_det          = ', N_det
    print *,  'N_states       = ', N_states
    do  k = 1, N_states
    print*,'State ',k
    print *,  'PT2            = ', pt2(k)
    print *,  'E              = ', CI_energy(k)
    print *,  'E(before)+PT2  = ', E_CI_before(k)+pt2(k)
    enddo
    print *,  '-----'
    E_CI_before = CI_energy
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
    E_CI_before = CI_energy
    call ezfio_set_full_ci_energy(CI_energy)
  enddo
   N_det = min(N_det_max,N_det)
   touch N_det psi_det psi_coef
   call diagonalize_CI
!    if(do_pt2_end)then
!     print*,'Last iteration only to compute the PT2'
!     threshold_selectors = 1.d0
!     threshold_generators = 0.999d0
!     call H_apply_FCI_PT2(pt2, norm_pert, H_pert_diag,  N_st)
!  
!     print *,  'Final step'
!     print *,  'N_det    = ', N_det
!     print *,  'N_states = ', N_states
!     print *,  'PT2      = ', pt2
!     print *,  'E        = ', CI_energy
!     print *,  'E+PT2    = ', CI_energy+pt2
!     print *,  '-----'
!     call ezfio_set_full_ci_energy_pt2(CI_energy+pt2)
!    endif
   call save_wavefunction
end




subroutine ZMQ_selection(N, pt2)
  use f77_zmq
  use selection_types
  
  implicit none
  
  character*(512)                :: task 
  integer(ZMQ_PTR)               :: zmq_to_qp_run_socket 
  integer, intent(in)            :: N
  type(selection_buffer)         :: b
  integer                        :: i
  integer, external              :: omp_get_thread_num
  double precision, intent(out)  :: pt2(N_states)
  !call flip_generators()
  call new_parallel_job(zmq_to_qp_run_socket,'selection')
  
  call create_selection_buffer(N, N*2, b)
  do i= N_det_generators, 1, -1
    write(task,*) i, N
    call add_task_to_taskserver(zmq_to_qp_run_socket,task)
  end do

  provide nproc
  provide ci_electronic_energy
  !$OMP PARALLEL DEFAULT(none)  SHARED(b, pt2)  PRIVATE(i) NUM_THREADS(nproc+1)
      i = omp_get_thread_num()
      if (i==0) then
        call selection_collector(b, pt2)
      else
        call selection_dressing_slave_inproc(i)
      endif
  !$OMP END PARALLEL
  call end_parallel_job(zmq_to_qp_run_socket, 'selection') 
  !call flip_generators()
  call fill_H_apply_buffer_no_selection(b%cur,b%det,N_int,0) !!! PAS DE ROBIN
  call copy_H_apply_buffer_to_wf()
end subroutine


subroutine selection_dressing_slave_tcp(i)
  implicit none
  integer, intent(in)            :: i

  call selection_slave(0,i)
end


subroutine selection_dressing_slave_inproc(i)
  implicit none
  integer, intent(in)            :: i

  call selection_slave(1,i)
end



! subroutine ZMQ_selection()
!   use f77_zmq
!   implicit none
!   BEGIN_DOC
! ! Massively parallel Full-CI
!   END_DOC
! 
!   integer :: i,ithread
!   integer(ZMQ_PTR) :: zmq_socket_push
!   integer(ZMQ_PTR), external :: new_zmq_push_socket
!   zmq_context = f77_zmq_ctx_new ()
!   PROVIDE H_apply_buffer_allocated
!   
!     PROVIDE ci_electronic_energy
!     PROVIDE nproc
!     !$OMP PARALLEL PRIVATE(i,ithread,zmq_socket_push) num_threads(nproc+1)
!     ithread = omp_get_thread_num()
!     if (ithread == 0) then
!       call receive_selected_determinants()
!     else
!       zmq_socket_push = new_zmq_push_socket(1)
!       
!       do i=ithread,N_det_generators,nproc
!         print *, i, "/", N_det_generators
!         call select_connected(i, max(100, N_det), ci_electronic_energy,zmq_socket_push)
!       enddo
!       
!       if (ithread == 1) then
!         integer :: rc
!         rc = f77_zmq_send(zmq_socket_push,0,1,0)
!         if (rc /= 1) then
!           stop 'Error sending termination signal'
!         endif
!       endif
!       call end_zmq_push_socket(zmq_socket_push, 1)
!     endif
!     !$OMP END PARALLEL
!     call copy_H_apply_buffer_to_wf()
! end













! program Full_CI_ZMQ
!   use f77_zmq
!   implicit none
!   BEGIN_DOC
! ! Massively parallel Full-CI
!   END_DOC
! 
!   integer :: i,ithread
! 
!   integer(ZMQ_PTR) :: zmq_socket_push
!   integer(ZMQ_PTR), external :: new_zmq_push_socket
!   zmq_context = f77_zmq_ctx_new ()
!   PROVIDE H_apply_buffer_allocated
!   
!   do while (N_det < N_det_max)
! 
!     PROVIDE ci_electronic_energy
!     PROVIDE nproc
!     !$OMP PARALLEL PRIVATE(i,ithread,zmq_socket_push) num_threads(nproc+1)
!     ithread = omp_get_thread_num()
!     if (ithread == 0) then
!       call receive_selected_determinants()
!     else
!       zmq_socket_push = new_zmq_push_socket(0)
!       
!       do i=ithread,N_det_generators,nproc
!         print *,  i , "/", N_det_generators
!         call select_connected(i, 1.d-7, ci_electronic_energy,zmq_socket_push)
!       enddo
!       print *, "END .... "
!       
!       if (ithread == 1) then
!         integer :: rc
!         rc = f77_zmq_send(zmq_socket_push,0,1,0)
!         if (rc /= 1) then
!           stop 'Error sending termination signal'
!         endif
!       endif
!       call end_zmq_push_socket(zmq_socket_push, 0)
!     endif
!     !$OMP END PARALLEL
!     call copy_H_apply_buffer_to_wf()
!     call diagonalize_CI()
!     call save_wavefunction()
!   end do    
! 
! end
