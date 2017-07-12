program create_wf
  read_wf = .true.
  SOFT_TOUCH read_wf
  PROVIDE H_apply_buffer_allocated
  call generate_all_alpha_beta_det_products
  call diagonalize_ci
  call save_wavefunction
end
