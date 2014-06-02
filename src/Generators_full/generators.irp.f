use bitmasks

BEGIN_PROVIDER [ double precision, threshold_generators ]
 implicit none
 BEGIN_DOC
 ! Percentage of the norm of the state-averaged wave function to
 ! consider for the generators
 END_DOC
 logical                        :: exists
 PROVIDE ezfio_filename
 call ezfio_has_determinants_threshold_generators(exists)
 if (exists) then
   call ezfio_get_determinants_threshold_generators(threshold_generators)
 else
   threshold_generators = 0.99d0
   call ezfio_set_determinants_threshold_generators(threshold_generators)
 endif
 ASSERT (N_det > 0)
 call write_double(output_Dets,threshold_generators,'Threshold on generators')
END_PROVIDER


BEGIN_PROVIDER [ integer, N_det_generators ]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the number of generators is 1 : the
 ! Hartree-Fock determinant
 END_DOC
 integer :: i
 double precision :: norm
 call write_time(output_dets)
 norm = 0.d0
 N_det_generators = N_det
 do i=1,N_det
   norm = norm + psi_average_norm_contrib_sorted(i)
   if (norm > threshold_generators) then
     N_det_generators = i-1
     exit
   endif
 enddo
 N_det_generators = max(N_det_generators,1)
 call write_int(output_dets,N_det_generators,'Number of generators')
END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_generators, (N_int,2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the generator is the
 ! Hartree-Fock determinant
 END_DOC
 psi_generators = psi_det_sorted

END_PROVIDER

 BEGIN_PROVIDER [ double precision, select_max, (3000) ]
 implicit none
 BEGIN_DOC
 ! Memo to skip useless selectors
 END_DOC
 select_max = huge(1.d0)
END_PROVIDER

