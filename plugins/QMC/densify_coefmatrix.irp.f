program densify
  implicit none
  read_wf = .True.
  touch read_wf
  call generate_all_alpha_beta_det_products()
  call diagonalize_ci
  call save_wavefunction
end
