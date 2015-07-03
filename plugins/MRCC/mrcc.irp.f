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

  integer :: i,j
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  psi_cas_coef(i,:)
    call debug_det(psi_cas(1,1,i),N_int)
  enddo

!  print *,  'SD'
!  print *,  '=='
!  do i=1,N_det_non_cas
!    print *,  psi_non_cas_coef(i,:)
!    call debug_det(psi_non_cas(1,1,i),N_int)
!  enddo
  call write_double(6,ci_energy(1),"Initial CI energy")
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

  double precision :: E_new, E_old, delta_e
  integer :: iteration
  E_new = 0.d0
  delta_E = 1.d0
  iteration = 0
  do while (delta_E > 1.d-10)
    iteration += 1
    print *,  '===========================' 
    print *,  'MRCC Iteration', iteration
    print *,  '===========================' 
    print *,  ''
    E_old = sum(ci_energy_dressed)
    call write_double(6,ci_energy_dressed(1),"MRCC energy")
    call diagonalize_ci_dressed
    E_new = sum(ci_energy_dressed)
    delta_E = dabs(E_new - E_old)
    if (iteration > 20) then
      exit
    endif
  enddo
  call write_double(6,ci_energy_dressed(1),"Final MRCC energy")
  call ezfio_set_mrcc_energy(ci_energy_dressed(1))
!  call save_wavefunction

end
