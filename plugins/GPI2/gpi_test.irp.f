program test
  double precision :: energy(N_states)
  if (is_gaspi_master) then
    energy = 1.d0
  else
    energy = 0.d0
  endif
  call broadcast_wf(energy)
  print *,  'energy (1.d0) :', GASPI_rank, energy(1)
  print *,  'coef          :', GASPI_rank, psi_coef(1,1)
  print *,  'det           :', GASPI_rank, psi_det (1,1,1)
  call gaspi_finalize
end
