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
  call save_wavefunction

end
