program densify
  implicit none
  read_wf = .True.
  touch read_wf
  call generate_all_alpha_beta_det_products()

!  call wf_of_psi_bilinear_matrix(.False.)
!  integer :: i, istate
!  do istate=1,N_states
!      do i=1,N_det
!        if (psi_coef(i,istate) == 0.d0) then
!          psi_coef(i,istate) = 1.d-6
!        endif
!      enddo
!  enddo
  call diagonalize_ci
  call save_wavefunction
end
