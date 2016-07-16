program diag_and_save
 implicit none
 read_wf = .True.
 touch read_wf
 call routine
end

subroutine routine
 implicit none
 call diagonalize_CI
 print*,'N_det = ',N_det
 call save_wavefunction_general(N_det,N_states_diag,psi_det_sorted,size(psi_coef_sorted,1),psi_coef_sorted)



end
