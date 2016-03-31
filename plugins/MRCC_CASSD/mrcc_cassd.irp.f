program mrcc
  implicit none
  read_wf = .True.
  SOFT_TOUCH read_wf
  call print_cas_coefs
  call set_generators_bitmasks_as_holes_and_particles
  call run
end

subroutine run
  implicit none
  
  integer :: i

  double precision :: E_new, E_old, delta_e
  integer :: iteration
  double precision :: E_past(4), lambda
  E_new = 0.d0
  delta_E = 1.d0
  iteration = 0
  lambda = 1.d0
  do while (delta_E > thresh_mrcc)
    iteration += 1
    print *,  '===========================' 
    print *,  'MRCC Iteration', iteration
    print *,  '===========================' 
    print *,  ''
    E_old = sum(ci_energy_dressed)
    call write_double(6,ci_energy_dressed(1),"MRCC energy")
    call diagonalize_ci_dressed(lambda)
    E_new = sum(ci_energy_dressed)
    delta_E = dabs(E_new - E_old)
    call save_wavefunction
    call ezfio_set_mrcc_cassd_energy(ci_energy_dressed(1))
    if (iteration > n_it_mrcc_max) then
      exit
    endif
  enddo
  call write_double(6,ci_energy_dressed(1),"Final MRCC energy")

end


subroutine print_cas_coefs
  implicit none

  integer :: i,j
  print *,  'CAS'
  print *,  '==='
  do i=1,N_det_cas
    print *,  psi_cas_coef(i,:)
    call debug_det(psi_cas(1,1,i),N_int)
  enddo
  call write_double(6,ci_energy(1),"Initial CI energy")

end

