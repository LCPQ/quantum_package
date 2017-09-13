program H_CORE_guess_prog
  BEGIN_DOC
! Produce `H_core` MO orbital 
! output:  mo_basis.mo_tot_num mo_basis.mo_label mo_basis.ao_md5 mo_basis.mo_coef mo_basis.mo_occ
  END_DOC
  implicit none
  character*(64)                 :: label
  call hcore_guess 
end
