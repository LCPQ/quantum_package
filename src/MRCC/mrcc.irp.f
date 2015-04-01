program mrcc
  implicit none
  read_wf = .True.
  TOUCH read_wf


  print *, N_det
  print *, N_det_cas
  print *, N_det_sd
!  psi_cas, (N_int,2,N_det_generators) ]
!psi_cas_coefs,  (N_det_generators,n_states) ]
!psi_sd,  (N_int,2,psi_det_size) ]
!psi_sd_coefs, (psi_det_size,n_states) ]

  call update_generators
  integer :: i
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  psi_cas_coefs(i,:)
    call debug_det(psi_cas(1,1,i),N_int)
  enddo

  print *,  'SD'
  print *,  '=='
  do i=1,N_det_sd
    print *,  psi_sd_coefs(i,:)
    call debug_det(psi_sd(1,1,i),N_int)
  enddo

  double precision, allocatable  :: pt2(:), norm_pert(:), H_pert_diag(:)
  integer                        :: N_st, degree
  N_st = N_states
  allocate (pt2(N_st), norm_pert(N_st),H_pert_diag(N_st))

  print *,  'MRCC'
  print *,  '===='
  call H_apply_mrcc(pt2, norm_pert, H_pert_diag,  N_st)
end
