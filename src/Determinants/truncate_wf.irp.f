program s2_eig_restart
 implicit none
 read_wf = .True.
 call routine
end
subroutine routine
 implicit none
  call make_s2_eigenfunction
  TOUCH psi_det psi_coef psi_det_sorted psi_coef_sorted psi_average_norm_contrib_sorted N_det
  call save_wavefunction
end
