subroutine send_result_energy(s_ndet,s_pt2,s_ci_energy)
  implicit none
  BEGIN_DOC
  !Send the result to the f77
  !Warning : Don t handle multi states pt2
  !No provider for the PT2, so this is a function with all the parameter
  END_DOC

  integer :: s_ndet
  double precision :: s_pt2, s_ci_energy

  integer(ZMQ_PTR)        :: msg
  integer                 :: rc

    msg   = f77_zmq_msg_new()
    rc = f77_zmq_msg_init_size(msg,6)
    rc = f77_zmq_msg_copy_to_data(msg, 'energy',6)
    rc = f77_zmq_msg_send(msg,responder,ZMQ_SNDMORE)
    rc = f77_zmq_msg_close(msg)

    rc = f77_zmq_msg_init_size(msg,4)
    rc = f77_zmq_msg_copy_to_data(msg, s_ndet, 4)
    rc = f77_zmq_msg_send(msg,responder,ZMQ_SNDMORE)
    rc = f77_zmq_msg_close(msg)

    rc = f77_zmq_msg_init_size(msg,8)
    rc = f77_zmq_msg_copy_to_data(msg, s_ci_energy, 8)
    rc = f77_zmq_msg_send(msg,responder,ZMQ_SNDMORE)
    rc = f77_zmq_msg_close(msg)

    rc = f77_zmq_msg_init_size(msg,8)
    rc = f77_zmq_msg_copy_to_data(msg, s_pt2, 8)
    rc = f77_zmq_msg_send(msg,responder,0)
    rc = f77_zmq_msg_close(msg)

    rc = f77_zmq_msg_destroy(msg)
    return
end

subroutine print_result_energy(r_ndet,r_ntates,r_pt2,r_ci_energy)
  implicit none
  BEGIN_DOC
  !Print and send to zmq if needed the energy
  !No provider for the PT2, so this is a function with all the parameter
  END_DOC

  integer :: r_ndet, r_ntates
  double precision :: r_pt2, r_ci_energy

    print *,  'N_det    = ', r_ndet
    print *,  'N_states = ', r_ntates
    print *,  'PT2      = ', r_pt2
    print *,  'E        = ', r_ci_energy
    print *,  'E+PT2    = ', r_ci_energy+r_pt2
    print *,  '-----'

    if (zmq_send == .True.) then
        call send_result_energy(r_ndet,r_pt2,r_ci_energy)
    endif

    return
end

program full_ci
  implicit none
  integer                        :: i,k

  
  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))
  character*(64)                 :: perturbation
  
  pt2 = 1.d0
  diag_algorithm = "Lapack"
  if (N_det > n_det_max_fci) then
    call diagonalize_CI
    call save_wavefunction
    psi_det = psi_det_sorted
    psi_coef = psi_coef_sorted
    N_det = n_det_max_fci
    soft_touch N_det psi_det psi_coef
    call diagonalize_CI
    call save_wavefunction

    call print_result_energy(N_det,N_states,pt2,CI_energy)
  endif


       

  do while (N_det < n_det_max_fci.and.maxval(abs(pt2(1:N_st))) > pt2_max)
    call H_apply_FCI(pt2, norm_pert, H_pert_diag,  N_st)

    PROVIDE  psi_coef
    PROVIDE  psi_det
    PROVIDE  psi_det_sorted

    if (N_det > n_det_max_fci) then
       psi_det = psi_det_sorted
       psi_coef = psi_coef_sorted
       N_det = n_det_max_fci
       soft_touch N_det psi_det psi_coef
    endif
    call diagonalize_CI
    call save_wavefunction

    call print_result_energy(N_det,N_states,pt2,CI_energy)



    if (abort_all) then
      exit
    endif
  enddo
   N_det = min(n_det_max_fci,N_det)
   if(do_pt2_end)then
    threshold_selectors = 1.d0
    threshold_generators = 0.999d0
    touch N_det psi_det psi_coef
    call diagonalize_CI
    call H_apply_FCI_PT2(pt2, norm_pert, H_pert_diag,  N_st)
 
    print *,  'Final step'
!!  call remove_small_contributions
!!  call diagonalize_CI

    call print_result_energy(N_det,N_states,pt2,CI_energy)
   endif
  deallocate(pt2,norm_pert)
end
