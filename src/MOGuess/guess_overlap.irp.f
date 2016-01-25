program guess_mimi
  BEGIN_DOC
! Produce `H_core` MO orbital
  END_DOC
  implicit none
  character*(64)                 :: label

  label = "Guess"
  call mo_as_eigvectors_of_mo_matrix(ao_overlap,          &
                                     size(ao_overlap,1),  &
                                     size(ao_overlap,2),label,-1)
  call save_mos
end
