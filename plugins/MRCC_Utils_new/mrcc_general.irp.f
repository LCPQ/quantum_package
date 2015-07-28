subroutine run_mrcc
  implicit none
  call set_generators_bitmasks_as_holes_and_particles
  call mrcc_iterations
end

subroutine mrcc_iterations
  implicit none
  
  integer :: i,j

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
    call write_double(6,ci_energy_dressed(1),"MRCC energy")
    call diagonalize_ci_dressed
    E_new = sum(ci_energy_dressed)
    delta_E = dabs(E_new - E_old)
!   stop
    if (iteration > 200) then
      exit
    endif
  enddo
  call write_double(6,ci_energy_dressed(1),"Final MRCC energy")
  call ezfio_set_mrcc_cassd_energy(ci_energy_dressed(1))
  call save_wavefunction

end

subroutine set_generators_bitmasks_as_holes_and_particles
 implicit none
 integer :: i,k
 do k = 1, N_generators_bitmask
  do i = 1, N_int
   ! Pure single part 
   generators_bitmask(i,1,1,k) = holes_operators(i,1)   ! holes for pure single exc alpha 
   generators_bitmask(i,1,2,k) = particles_operators(i,1) ! particles for pure single exc alpha 
   generators_bitmask(i,2,1,k) = holes_operators(i,2)   ! holes for pure single exc beta 
   generators_bitmask(i,2,2,k) = particles_operators(i,2) ! particles for pure single exc beta 

   ! Double excitation 
   generators_bitmask(i,1,3,k) = holes_operators(i,1)   ! holes for first single exc alpha 
   generators_bitmask(i,1,4,k) = particles_operators(i,1) ! particles for first single exc alpha 
   generators_bitmask(i,2,3,k) = holes_operators(i,2)   ! holes for first single exc beta 
   generators_bitmask(i,2,4,k) = particles_operators(i,2) ! particles for first single exc beta 

   generators_bitmask(i,1,5,k) = holes_operators(i,1)   ! holes for second single exc alpha 
   generators_bitmask(i,1,6,k) = particles_operators(i,1) ! particles for second single exc alpha 
   generators_bitmask(i,2,5,k) = holes_operators(i,2)   ! holes for second single exc beta 
   generators_bitmask(i,2,6,k) = particles_operators(i,2) ! particles for second single exc beta 

  enddo
 enddo
 touch generators_bitmask



end
