use bitmasks

BEGIN_SHELL [ /usr/bin/python ]
from ezfio_with_default import EZFIO_Provider
T = EZFIO_Provider()
T.set_type      ( "double precision" )
T.set_name      ( "threshold_selectors" )
T.set_doc       ( "Percentage of the norm of the state-averaged wave function to consider for the selectors" )
T.set_ezfio_dir ( "determinants" )
T.set_ezfio_name( "threshold_selectors" )
T.set_output    ( "output_dets" )
print T
END_SHELL

BEGIN_PROVIDER [ integer, psi_selectors_size ]
 implicit none
 psi_selectors_size = psi_det_size
END_PROVIDER

BEGIN_PROVIDER [ integer, N_det_selectors]
 implicit none
 BEGIN_DOC
 ! For Single reference wave functions, the number of selectors is 1 : the
 ! Hartree-Fock determinant
 END_DOC
 integer :: i
 double precision :: norm
 call write_time(output_dets)
 norm = 0.d0
 N_det_selectors = N_det
 do i=1,N_det
   norm = norm + psi_average_norm_contrib_sorted(i)
   if (norm > threshold_selectors) then
     N_det_selectors = i-1
     exit
   endif
 enddo
 N_det_selectors = max(N_det_selectors,1)
 call write_int(output_dets,N_det_selectors,'Number of selectors')
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_selectors, (N_int,2,psi_selectors_size) ]
&BEGIN_PROVIDER [ double precision, psi_selectors_coef, (psi_selectors_size,N_states) ]
  implicit none
  BEGIN_DOC
  ! Determinants on which we apply <i|H|psi> for perturbation.
  END_DOC
  integer                        :: i,k

  do i=1,N_det_selectors
    do k=1,N_int
      psi_selectors(k,1,i) = psi_det_sorted(k,1,i)
      psi_selectors(k,2,i) = psi_det_sorted(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det_selectors
      psi_selectors_coef(i,k) = psi_coef_sorted(i,k)
    enddo
  enddo
END_PROVIDER

