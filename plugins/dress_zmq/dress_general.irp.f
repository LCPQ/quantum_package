subroutine run_dressing(N_st,energy)
  implicit none
  
  integer, intent(in) :: N_st 
  double precision, intent(out) :: energy(N_st) 

  integer :: i,j

  double precision :: E_new, E_old, delta_e
  integer :: iteration
  
  integer :: n_it_dress_max
  double precision :: thresh_dress

  thresh_dress = thresh_dressed_ci
  n_it_dress_max = n_it_max_dressed_ci

  if(n_it_dress_max == 1) then
    do j=1,N_states
      do i=1,N_det
        psi_coef(i,j) = CI_eigenvectors_dressed(i,j)
      enddo
    enddo
    SOFT_TOUCH psi_coef ci_energy_dressed
    call write_double(6,ci_energy_dressed(1),"Final dress energy")
!   call ezfio_set_dress_zmq_energy(ci_energy_dressed(1))
    call save_wavefunction
  else
    E_new = 0.d0
    delta_E = 1.d0
    iteration = 0
    do while (delta_E > thresh_dress)
      iteration += 1
      print *,  '===============================================' 
      print *,  'Iteration', iteration, '/', n_it_dress_max
      print *,  '===============================================' 
      print *,  ''
      E_old = dress_e0_denominator(1) !sum(ci_energy_dressed(1:N_states))
      do i=1,N_st
        call write_double(6,ci_energy_dressed(i),"Energy")
      enddo
      call diagonalize_ci_dressed
      E_new = dress_e0_denominator(1) !sum(ci_energy_dressed(1:N_states))

      delta_E = (E_new - E_old)/dble(N_states)
      print *,  ''
      call write_double(6,thresh_dress,"thresh_dress")
      call write_double(6,delta_E,"delta_E")
      delta_E = dabs(delta_E)
      call save_wavefunction
!     call ezfio_set_dress_zmq_energy(ci_energy_dressed(1))
      if (iteration >= n_it_dress_max) then
        exit
      endif
    enddo
    call write_double(6,ci_energy_dressed(1),"Final energy")
  endif
  energy(1:N_st) = ci_energy_dressed(1:N_st)
end

