BEGIN_PROVIDER [ integer, N_det_max_fci ]
 implicit none
 BEGIN_DOC
! Max number od determinants in the wave function
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_full_ci_n_det_max_fci(exists)
 if (exists) then
   call ezfio_get_full_ci_n_det_max_fci(n_det_max_fci)
 else
   n_det_max_fci = 10000
 endif
 call write_int(output_full_ci,n_det_max_fci,'Max number of determinants ')
 ASSERT (n_det_max_fci > 0)
END_PROVIDER


BEGIN_PROVIDER [ logical , do_pt2_end ]
 implicit none
 BEGIN_DOC
! if True then compute the PT2 when the selection process is finished
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_full_ci_do_pt2_end(exists)
 if (exists) then
   call ezfio_get_full_ci_do_pt2_end(do_pt2_end)
 else
   do_pt2_end = .True.
 endif
!call write_i(output_full_ci,do_pt2_end,' computes the PT2 at the end of the selection ')
 ASSERT (do_pt2_end > 0)
END_PROVIDER


BEGIN_PROVIDER [ double precision , pt2_max ]
 implicit none
 BEGIN_DOC
! The selection process stops when the largest PT2 (for all the states) is lower than pt2_max
! in absolute value
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_full_ci_pt2_max(exists)
 if (exists) then
   call ezfio_get_full_ci_pt2_max(pt2_max)
 else
   pt2_max = 0.1d0
 endif
END_PROVIDER
