BEGIN_PROVIDER [ logical , read_wf ]
 implicit none
 BEGIN_DOC
! If true, read the wave function from the EZFIO file
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_determinants_read_wf(exists)
 if (exists) then
   call ezfio_get_determinants_read_wf(read_wf)
 else
   read_wf = .True.
 endif
!call write_i(output_determinants,read_wf,' computes the PT2 at the end of the selection ')
 ASSERT (read_wf > 0)
END_PROVIDER

