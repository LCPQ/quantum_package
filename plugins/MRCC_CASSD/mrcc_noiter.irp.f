program mrcc_noiter
  implicit none
  read_wf = .True.
  SOFT_TOUCH read_wf
  call print_cas_coefs
  call set_generators_bitmasks_as_holes_and_particles
  call run
end

subroutine run
  implicit none
  
  double precision :: lambda
  integer :: i,j
  do j=1,N_states_diag
    do i=1,N_det
      psi_coef(i,j) = CI_eigenvectors_dressed(i,j) 
    enddo
  enddo
  SOFT_TOUCH psi_coef ci_energy_dressed
  call write_double(6,ci_energy_dressed(1),"Final MRCC energy")
  call ezfio_set_mrcc_cassd_energy(ci_energy_dressed(1))
  call save_wavefunction

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

