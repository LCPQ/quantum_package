  BEGIN_PROVIDER [ integer, n_det_max_cisd_sc2 ]
   implicit none
   BEGIN_DOC
  ! Get n_det_max_cisd_sc2 from EZFIO file
   END_DOC
   logical :: has_n_det_max_cisd_sc2
   PROVIDE ezfio_filename
   call ezfio_has_cisd_sc2_selected_n_det_max_cisd_sc2(has_n_det_max_cisd_sc2)
   if (has_n_det_max_cisd_sc2) then
     call ezfio_get_cisd_sc2_selected_n_det_max_cisd_sc2(n_det_max_cisd_sc2)
   else
     n_det_max_cisd_sc2 = 1000
     call ezfio_set_cisd_sc2_selected_n_det_max_cisd_sc2(n_det_max_cisd_sc2)
   endif
   print*,'n_det_max_cisd_sc2 = ',n_det_max_cisd_sc2
  END_PROVIDER

  BEGIN_PROVIDER [ double precision , pt2_max ]
   implicit none
   BEGIN_DOC
  ! Get pt2_max from EZFIO file
   END_DOC
   logical :: has_pt2_max
   PROVIDE ezfio_filename
   call ezfio_has_cisd_sc2_selected_pt2_max(has_pt2_max)
   if (has_pt2_max) then
     call ezfio_get_cisd_sc2_selected_pt2_max(pt2_max)
   else
     pt2_max = 1.d-3
     call ezfio_set_cisd_sc2_selected_pt2_max(pt2_max)
   endif
   print*,'pt2_max = ',pt2_max
  END_PROVIDER

  BEGIN_PROVIDER [ logical, do_pt2_end ]
   implicit none
   BEGIN_DOC
  ! Get do_pt2_end from EZFIO file
   END_DOC
   logical :: has_do_pt2_end
   PROVIDE ezfio_filename
   call ezfio_has_cisd_sc2_selected_do_pt2_end(has_do_pt2_end)
   if (has_do_pt2_end) then
     call ezfio_get_cisd_sc2_selected_do_pt2_end(do_pt2_end)
   else
     do_pt2_end = .True.
     call ezfio_set_cisd_sc2_selected_do_pt2_end(do_pt2_end)
   endif
   print*,'do_pt2_end = ',do_pt2_end
  END_PROVIDER

