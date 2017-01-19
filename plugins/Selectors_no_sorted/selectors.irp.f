
use bitmasks


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
 call write_time(output_determinants)
 norm = 0.d0
 N_det_selectors = N_det
 N_det_selectors = max(N_det_selectors,1)
 call write_int(output_determinants,N_det_selectors,'Number of selectors')
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
      psi_selectors(k,1,i) = psi_det(k,1,i)
      psi_selectors(k,2,i) = psi_det(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det_selectors
      psi_selectors_coef(i,k) = psi_coef(i,k)
    enddo
  enddo
END_PROVIDER

 BEGIN_PROVIDER [ double precision, psi_selectors_diag_h_mat, (psi_selectors_size) ]
  implicit none
  BEGIN_DOC
  ! Diagonal elements of the H matrix for each selectors 
  END_DOC
  integer                        :: i
  double precision :: diag_H_mat_elem
  do i = 1, N_det_selectors
   psi_selectors_diag_h_mat(i) = diag_H_mat_elem(psi_selectors(1,1,i),N_int)
  enddo
 END_PROVIDER


