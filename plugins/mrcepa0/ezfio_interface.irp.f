! DO NOT MODIFY BY HAND
! Created by $QP_ROOT/scripts/ezfio_interface/ei_handler.py
! from file /home/garniron/quantum_package/src/mrcepa0/EZFIO.cfg


BEGIN_PROVIDER [ logical, perturbative_triples  ]
  implicit none
  BEGIN_DOC
! Compute perturbative contribution of the Triples
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrcepa0_perturbative_triples(has)
  if (has) then
    call ezfio_get_mrcepa0_perturbative_triples(perturbative_triples)
  else
    print *, 'mrcepa0/perturbative_triples not found in EZFIO file'
    stop 1
  endif

END_PROVIDER

BEGIN_PROVIDER [ double precision, thresh_dressed_ci  ]
  implicit none
  BEGIN_DOC
! Threshold on the convergence of the dressed CI energy
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrcepa0_thresh_dressed_ci(has)
  if (has) then
    call ezfio_get_mrcepa0_thresh_dressed_ci(thresh_dressed_ci)
  else
    print *, 'mrcepa0/thresh_dressed_ci not found in EZFIO file'
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
  
  call ezfio_has_mrcepa0_n_it_max_dressed_ci(has)
  if (has) then
    call ezfio_get_mrcepa0_n_it_max_dressed_ci(n_it_max_dressed_ci)
  else
    print *, 'mrcepa0/n_it_max_dressed_ci not found in EZFIO file'
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
  
  call ezfio_has_mrcepa0_lambda_type(has)
  if (has) then
    call ezfio_get_mrcepa0_lambda_type(lambda_type)
  else
    print *, 'mrcepa0/lambda_type not found in EZFIO file'
    stop 1
  endif

END_PROVIDER
