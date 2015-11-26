subroutine hcore_guess
  BEGIN_DOC
! Produce `H_core` MO orbital
  END_DOC
  implicit none
  character*(64)                 :: label
  mo_coef = ao_ortho_lowdin_coef
  TOUCH mo_coef
  label = "Guess"
  call mo_as_eigvectors_of_mo_matrix(mo_mono_elec_integral,          &
                                     size(mo_mono_elec_integral,1),  &
                                     size(mo_mono_elec_integral,2),label,1)
  print *,  'save mos'
  call save_mos
  SOFT_TOUCH mo_coef mo_label
end
