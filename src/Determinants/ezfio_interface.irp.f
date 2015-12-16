! DO NOT MODIFY BY HAND
! Created by $QP_ROOT/scripts/ezfio_interface.py
! from file /home/garniron/quantum_package/src/Determinants/EZFIO.cfg


BEGIN_PROVIDER [ double precision, threshold_selectors  ]
  implicit none
  BEGIN_DOC
! Thresholds on selectors (fraction of the norm)
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_threshold_selectors(has)
  if (has) then
    call ezfio_get_determinants_threshold_selectors(threshold_selectors)
  else
    print *, 'determinants/threshold_selectors not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_double(output_determinants, threshold_selectors, &
     'threshold_selectors')

END_PROVIDER

BEGIN_PROVIDER [ double precision, expected_s2  ]
  implicit none
  BEGIN_DOC
! Expected value of S^2
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_expected_s2(has)
  if (has) then
    call ezfio_get_determinants_expected_s2(expected_s2)
  else
    print *, 'determinants/expected_s2 not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_double(output_determinants, expected_s2, &
     'expected_s2')

END_PROVIDER

BEGIN_PROVIDER [ integer, n_det_max  ]
  implicit none
  BEGIN_DOC
! Max number of determinants in the wave function
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_n_det_max(has)
  if (has) then
    call ezfio_get_determinants_n_det_max(n_det_max)
  else
    print *, 'determinants/n_det_max not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_int(output_determinants, n_det_max, &
     'n_det_max')

END_PROVIDER

BEGIN_PROVIDER [ integer, n_states  ]
  implicit none
  BEGIN_DOC
! Number of states to consider
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_n_states(has)
  if (has) then
    call ezfio_get_determinants_n_states(n_states)
  else
    print *, 'determinants/n_states not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_int(output_determinants, n_states, &
     'n_states')

END_PROVIDER

BEGIN_PROVIDER [ integer, n_det_max_jacobi  ]
  implicit none
  BEGIN_DOC
! Maximum number of determinants diagonalized by Jacobi
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_n_det_max_jacobi(has)
  if (has) then
    call ezfio_get_determinants_n_det_max_jacobi(n_det_max_jacobi)
  else
    print *, 'determinants/n_det_max_jacobi not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_int(output_determinants, n_det_max_jacobi, &
     'n_det_max_jacobi')

END_PROVIDER

BEGIN_PROVIDER [ logical, read_wf  ]
  implicit none
  BEGIN_DOC
! If true, read the wave function from the EZFIO file
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_read_wf(has)
  if (has) then
    call ezfio_get_determinants_read_wf(read_wf)
  else
    print *, 'determinants/read_wf not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_bool(output_determinants, read_wf, &
     'read_wf')

END_PROVIDER

BEGIN_PROVIDER [ double precision, det_coef , (n_det) ]
  implicit none
  BEGIN_DOC
! det_coef
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_det_coef(has)
  if (has) then
    call ezfio_get_determinants_det_coef(det_coef)
  else
    print *, 'determinants/det_coef not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_double(output_determinants, det_coef, &
     'det_coef')

END_PROVIDER

BEGIN_PROVIDER [ double precision, target_energy  ]
  implicit none
  BEGIN_DOC
! Energy that should be obtained when truncating the wave function (optional)
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_target_energy(has)
  if (has) then
    call ezfio_get_determinants_target_energy(target_energy)
  else
    print *, 'determinants/target_energy not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_double(output_determinants, target_energy, &
     'target_energy')

END_PROVIDER

BEGIN_PROVIDER [ logical, only_single_double_dm  ]
  implicit none
  BEGIN_DOC
! If true, The One body DM is calculated with ignoring the Double<->Doubles extra diag elements
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_only_single_double_dm(has)
  if (has) then
    call ezfio_get_determinants_only_single_double_dm(only_single_double_dm)
  else
    print *, 'determinants/only_single_double_dm not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_bool(output_determinants, only_single_double_dm, &
     'only_single_double_dm')

END_PROVIDER

BEGIN_PROVIDER [ double precision, threshold_davidson  ]
  implicit none
  BEGIN_DOC
! Thresholds of Davidson's algorithm
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_threshold_davidson(has)
  if (has) then
    call ezfio_get_determinants_threshold_davidson(threshold_davidson)
  else
    print *, 'determinants/threshold_davidson not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_double(output_determinants, threshold_davidson, &
     'threshold_davidson')

END_PROVIDER

BEGIN_PROVIDER [ integer, n_states_diag  ]
  implicit none
  BEGIN_DOC
! n_states_diag
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_n_states_diag(has)
  if (has) then
    call ezfio_get_determinants_n_states_diag(n_states_diag)
  else
    print *, 'determinants/n_states_diag not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_int(output_determinants, n_states_diag, &
     'n_states_diag')

END_PROVIDER

BEGIN_PROVIDER [ integer, det_occ , (elec_alpha_num,n_det,2) ]
  implicit none
  BEGIN_DOC
! det_occ
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_det_occ(has)
  if (has) then
    call ezfio_get_determinants_det_occ(det_occ)
  else
    print *, 'determinants/det_occ not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_int(output_determinants, det_occ, &
     'det_occ')

END_PROVIDER

BEGIN_PROVIDER [ logical, s2_eig  ]
  implicit none
  BEGIN_DOC
! Force the wave function to be an eigenfunction of S^2
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_s2_eig(has)
  if (has) then
    call ezfio_get_determinants_s2_eig(s2_eig)
  else
    print *, 'determinants/s2_eig not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_bool(output_determinants, s2_eig, &
     's2_eig')

END_PROVIDER

BEGIN_PROVIDER [ double precision, threshold_generators  ]
  implicit none
  BEGIN_DOC
! Thresholds on generators (fraction of the norm)
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_threshold_generators(has)
  if (has) then
    call ezfio_get_determinants_threshold_generators(threshold_generators)
  else
    print *, 'determinants/threshold_generators not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_double(output_determinants, threshold_generators, &
     'threshold_generators')

END_PROVIDER

BEGIN_PROVIDER [ integer, n_det_max_property  ]
  implicit none
  BEGIN_DOC
! Max number of determinants in the wave function when you select for a given property
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_n_det_max_property(has)
  if (has) then
    call ezfio_get_determinants_n_det_max_property(n_det_max_property)
  else
    print *, 'determinants/n_det_max_property not found in EZFIO file'
    stop 1
  endif

  call write_time(output_determinants)
  call write_int(output_determinants, n_det_max_property, &
     'n_det_max_property')

END_PROVIDER
