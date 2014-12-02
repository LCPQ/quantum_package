BEGIN_PROVIDER [ integer, n_det_max_fci ]
  implicit none
  BEGIN_DOC  
!  Max number of determinants in the wave function
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_full_ci_n_det_max_fci(has)
  if (has) then
    call ezfio_get_full_ci_n_det_max_fci(n_det_max_fci)
  else
    n_det_max_fci = 10000
  endif
  
  call write_time(output_full_ci)
  call write_int(output_full_ci, n_det_max_fci, &
      'n_det_max_fci')
      

END_PROVIDER

BEGIN_PROVIDER [ double precision, pt2_max ]
  implicit none
  BEGIN_DOC  
!  The selection process stops when the largest PT2 (for all the states) is lower than pt2_max in absolute value
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_full_ci_pt2_max(has)
  if (has) then
    call ezfio_get_full_ci_pt2_max(pt2_max)
  else
    pt2_max = 1.d-4
  endif
  
  call write_time(output_full_ci)
  call write_double(output_full_ci, pt2_max, &
      'pt2_max')
      

END_PROVIDER

BEGIN_PROVIDER [ logical, do_pt2_end ]
  implicit none
  BEGIN_DOC  
!  If true, compute the PT2 at the end of the selection
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_full_ci_do_pt2_end(has)
  if (has) then
    call ezfio_get_full_ci_do_pt2_end(do_pt2_end)
  else
    do_pt2_end = .True.
  endif
  
  call write_time(output_full_ci)
  call write_bool(output_full_ci, do_pt2_end, &
      'do_pt2_end')
      

END_PROVIDER