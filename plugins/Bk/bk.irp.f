program bk
  implicit none
  BEGIN_DOC
! Shifted-Bk method
  END_DOC
  read_wf = .True.
  state_following = .True.
  TOUCH read_wf state_following
  call run()
end

subroutine run
  implicit none
  call diagonalize_ci_dressed
  integer :: istate
  print *,  'Bk Energy'
  print *,  '---------'
  print *,  ''
  do istate = 1,N_states
   print *,  istate, CI_energy_dressed(istate)
  enddo
!  call save_wavefunction
  call ezfio_set_bk_energy(ci_energy_dressed(1))
end


