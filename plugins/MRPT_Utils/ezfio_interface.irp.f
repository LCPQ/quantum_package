! DO NOT MODIFY BY HAND
! Created by $QP_ROOT/scripts/ezfio_interface/ei_handler.py
! from file /home/giner/qp_bis/quantum_package/src/MRPT_Utils/EZFIO.cfg


BEGIN_PROVIDER [ logical, do_third_order_1h1p  ]
  implicit none
  BEGIN_DOC
! If true, compute the third order contribution for the 1h1p
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrpt_utils_do_third_order_1h1p(has)
  if (has) then
    call ezfio_get_mrpt_utils_do_third_order_1h1p(do_third_order_1h1p)
  else
    print *, 'mrpt_utils/do_third_order_1h1p not found in EZFIO file'
    stop 1
  endif

END_PROVIDER

BEGIN_PROVIDER [ logical, pure_state_specific_mrpt2  ]
  implicit none
  BEGIN_DOC
! If true, diagonalize the dressed matrix for each state and do a state following of the initial states
  END_DOC

  logical                        :: has
  PROVIDE ezfio_filename
  
  call ezfio_has_mrpt_utils_pure_state_specific_mrpt2(has)
  if (has) then
    call ezfio_get_mrpt_utils_pure_state_specific_mrpt2(pure_state_specific_mrpt2)
  else
    print *, 'mrpt_utils/pure_state_specific_mrpt2 not found in EZFIO file'
    stop 1
  endif

END_PROVIDER
