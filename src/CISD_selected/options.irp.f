  BEGIN_PROVIDER [ integer, n_det_max_cisd ]
   implicit none
   BEGIN_DOC
  ! Get n_det_max_cisd from EZFIO file
   END_DOC
   logical :: has_n_det_max_cisd
   PROVIDE ezfio_filename
   call ezfio_has_cisd_selected_n_det_max_cisd(has_n_det_max_cisd)
   if (has_n_det_max_cisd) then
     call ezfio_get_cisd_selected_n_det_max_cisd(n_det_max_cisd)
   else
     n_det_max_cisd = 30000
     call ezfio_set_cisd_selected_n_det_max_cisd(n_det_max_cisd)
   endif
   print*,'n_det_max_cisd = ',n_det_max_cisd
  END_PROVIDER

  BEGIN_PROVIDER [ double precision , pt2_max ]
   implicit none
   BEGIN_DOC
  ! Get pt2_max from EZFIO file
   END_DOC
   logical :: has_pt2_max
   PROVIDE ezfio_filename
   call ezfio_has_cisd_selected_pt2_max(has_pt2_max)
   if (has_pt2_max) then
     call ezfio_get_cisd_selected_pt2_max(pt2_max)
   else
     pt2_max = 1.d-9
     call ezfio_set_cisd_selected_pt2_max(pt2_max)
   endif
   print*,'pt2_max = ',pt2_max
  END_PROVIDER

