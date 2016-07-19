

program fci_zmq
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  
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
    provide selection_criterion
  if(read_wf)then
   call i_H_psi(psi_det(1,1,N_det),psi_det,psi_coef,N_int,N_det,psi_det_size,N_states,i_H_psi_array)
   h = diag_H_mat_elem(psi_det(1,1,N_det),N_int)
   selection_criterion = dabs(psi_coef(N_det,1) *  (i_H_psi_array(1) - h * psi_coef(N_det,1))) * 0.1d0
   soft_touch selection_criterion
  endif


  integer :: n_det_before
  print*,'Beginning the selection ...'
  E_CI_before = CI_energy
  do while (N_det < N_det_max.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    !selection_criterion = 1d-7
    print *, selection_criterion, "+++++++++++++++++++++++++++++++++++++++", N_det
    n_det_before = N_det
!     call H_apply_FCI(pt2, norm_pert, H_pert_diag,  N_st)
    call ZMQ_selection()
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
    if(n_det_before == N_det)then
     selection_criterion = selection_criterion * 0.1d0
    endif
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



subroutine ZMQ_selection()
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
  
    PROVIDE ci_electronic_energy
    PROVIDE nproc
    !$OMP PARALLEL PRIVATE(i,ithread,zmq_socket_push) num_threads(nproc+1)
    ithread = omp_get_thread_num()
    if (ithread == 0) then
      call receive_selected_determinants()
    else
      zmq_socket_push = new_zmq_push_socket(1)
      
      do i=ithread,N_det_generators,nproc
        print *, i, "/", N_det_generators
        call select_connected(i, max(100, N_det), ci_electronic_energy,zmq_socket_push)
      enddo
      
      if (ithread == 1) then
        integer :: rc
        rc = f77_zmq_send(zmq_socket_push,0,1,0)
        if (rc /= 1) then
          stop 'Error sending termination signal'
        endif
      endif
      call end_zmq_push_socket(zmq_socket_push, 1)
    endif
    !$OMP END PARALLEL
    call copy_H_apply_buffer_to_wf()
end













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
