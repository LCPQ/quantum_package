program mrcc
  implicit none
  read_wf = .True.
  TOUCH read_wf
  call run
  call run_mrcc
!  call run_mrcc_test
end

subroutine run
  implicit none

  print *, N_det
  print *, N_det_cas
  print *, N_det_sd

  integer :: i,j
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
  print *,  'xxx', 'Energy CAS+SD', ci_energy
end
subroutine run_mrcc_test
  implicit none
  integer :: i,j
  double precision :: pt2
  pt2 = 0.d0
  do j=1,N_det
   do i=1,N_det
     pt2 += psi_coef(i,1)*psi_coef(j,1) * delta_ij(i,j,1)
   enddo
  enddo
  print *,  ci_energy(1)
  print *,  ci_energy(1)+pt2
end
subroutine run_mrcc
  implicit none
  integer :: i,j
  print *,  'MRCC'
  print *,  '===='
  print *,  ''
  print *,  'CAS+SD energy : ', ci_energy_dressed(:)
  print *,  ''

!    call diagonalize_ci_dressed
!    call save_wavefunction_unsorted
  double precision :: E_new, E_old, delta_e
  integer :: iteration
  E_new = 0.d0
  delta_E = 1.d0
  iteration = 0
  do while (delta_E > 1.d-8)
    iteration += 1
    print *,  '===========================' 
    print *,  'MRCC Iteration', iteration
    print *,  '===========================' 
    print *,  ''
    E_old = sum(ci_energy_dressed)
    call diagonalize_ci_dressed
    E_new = sum(ci_energy_dressed)
    delta_E = dabs(E_new - E_old)
    call write_double(6,ci_energy_dressed(1),"MRCC energy")
  enddo

end
