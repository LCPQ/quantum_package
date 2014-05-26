use bitmasks
BEGIN_SHELL [ /bin/bash ]
./h_apply.py
END_SHELL


BEGIN_PROVIDER [ integer, psi_ref_size ]
 implicit none
 psi_ref_size = psi_det_size
END_PROVIDER
BEGIN_PROVIDER [ integer, N_det_ref]
 implicit none
  N_det_ref = N_det
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_ref, (N_int,2,psi_ref_size) ]
&BEGIN_PROVIDER [ double precision, psi_ref_coef, (psi_ref_size,N_states) ]
  implicit none
  BEGIN_DOC
  ! On what we apply <i|H|psi> for perturbation. If selection, it may be 0.9 of the norm.
  END_DOC
  integer                        :: i,k
  
  do i=1,N_det_ref
    do k=1,N_int
      psi_ref(k,1,i) = psi_det(k,1,i)
      psi_ref(k,2,i) = psi_det(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det_ref
      psi_ref_coef(i,k) = psi_coef(i,k)
    enddo
  enddo
END_PROVIDER


