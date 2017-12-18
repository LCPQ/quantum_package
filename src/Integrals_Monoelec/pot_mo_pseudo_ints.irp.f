BEGIN_PROVIDER [double precision, mo_pseudo_integral, (mo_tot_num,mo_tot_num)]
  implicit none
  BEGIN_DOC
  ! interaction nuclear electron on the MO basis
  END_DOC
  
  if (read_mo_one_integrals) then
    call read_one_e_integrals('mo_pseudo_integral', mo_pseudo_integral,&
        size(mo_pseudo_integral,1), size(mo_pseudo_integral,2))
    print *,  'MO pseudopotential integrals read from disk'
  else
    call ao_to_mo(                                                   &
        ao_pseudo_integral,                                         &
        size(ao_pseudo_integral,1),                                 &
        mo_pseudo_integral,                                         &
        size(mo_pseudo_integral,1)                                  &
        )
  endif
  if (write_mo_one_integrals) then
    call write_one_e_integrals('mo_pseudo_integral', mo_pseudo_integral,&
        size(mo_pseudo_integral,1), size(mo_pseudo_integral,2))
    print *,  'MO pseudopotential integrals written to disk'
  endif

END_PROVIDER


