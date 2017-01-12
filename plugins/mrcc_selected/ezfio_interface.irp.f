! DO NOT MODIFY BY HAND
! Created by $QP_ROOT/scripts/ezfio_interface/ei_handler.py
! from file /home/scemama/quantum_package/src/mrcc_selected/EZFIO.cfg


BEGIN_PROVIDER [ double precision, thresh_dressed_ci  ]
  implicit none
  BEGIN_DOC
! Threshold on the convergence of the dressed CI energy
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrcc_selected_thresh_dressed_ci(has)
  if (has) then
    call ezfio_get_mrcc_selected_thresh_dressed_ci(thresh_dressed_ci)
  else
    print *, 'mrcc_selected/thresh_dressed_ci not found in EZFIO file'
    stop 1
  endif

END_PROVIDER

BEGIN_PROVIDER [ integer, n_it_max_dressed_ci  ]
  implicit none
  BEGIN_DOC
! Maximum number of dressed CI iterations
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrcc_selected_n_it_max_dressed_ci(has)
  if (has) then
    call ezfio_get_mrcc_selected_n_it_max_dressed_ci(n_it_max_dressed_ci)
  else
    print *, 'mrcc_selected/n_it_max_dressed_ci not found in EZFIO file'
    stop 1
  endif

END_PROVIDER

BEGIN_PROVIDER [ integer, lambda_type  ]
  implicit none
  BEGIN_DOC
! lambda type
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrcc_selected_lambda_type(has)
  if (has) then
    call ezfio_get_mrcc_selected_lambda_type(lambda_type)
  else
    print *, 'mrcc_selected/lambda_type not found in EZFIO file'
    stop 1
  endif

END_PROVIDER
