BEGIN_PROVIDER [ integer , n_state_cis ]
  implicit none
  BEGIN_DOC
  !  Number of states asked for the CIS vector
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_cis_dressed_n_state_cis(has)
  if (has) then
    call ezfio_get_cis_dressed_n_state_cis(n_state_cis)
  else
    n_state_cis = 5
  endif

END_PROVIDER

BEGIN_PROVIDER [ integer , n_core_cis]
  implicit none
  BEGIN_DOC
  !  Number of states asked for the CIS vector
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_cis_dressed_n_core_cis(has)
  if (has) then
    call ezfio_get_cis_dressed_n_core_cis(n_core_cis)
  else
    n_core_cis = 0
  endif

END_PROVIDER

BEGIN_PROVIDER [ integer , n_act_cis]
  implicit none
  BEGIN_DOC
  !  Number of states asked for the CIS vector
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_cis_dressed_n_act_cis(has)
  if (has) then
    call ezfio_get_cis_dressed_n_act_cis(n_act_cis)
  else
    n_act_cis = mo_tot_num
  endif

END_PROVIDER

BEGIN_PROVIDER [ logical , mp2_dressing]
  implicit none
  BEGIN_DOC
  !  Number of states asked for the CIS vector
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_cis_dressed_mp2_dressing(has)
  if (has) then
    call ezfio_get_cis_dressed_mp2_dressing(mp2_dressing)
  else
    mp2_dressing = .False.
  endif

END_PROVIDER

BEGIN_PROVIDER [ logical , standard_doubles]
  implicit none
  BEGIN_DOC
  !  Number of states asked for the CIS vector
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_cis_dressed_standard_doubles(has)
  if (has) then
    call ezfio_get_cis_dressed_standard_doubles(standard_doubles)
  else
    standard_doubles = .True.
  endif

END_PROVIDER


BEGIN_PROVIDER [ logical , en_2_2]
  implicit none
  BEGIN_DOC
  !  Number of states asked for the CIS vector
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_cis_dressed_en_2_2(has)
  if (has) then
    call ezfio_get_cis_dressed_en_2_2(en_2_2)
  else
    en_2_2 = .False.
  endif

END_PROVIDER

