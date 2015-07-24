use bitmasks

 BEGIN_PROVIDER [ integer(bit_kind), psi_ref, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_ref_coef,  (psi_det_size,n_states) ]
&BEGIN_PROVIDER [ integer, idx_ref, (psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_det_ref ]
  implicit none
  BEGIN_DOC
  ! Reference wave function, defined as determinants with coefficients > 0.05
  ! idx_ref gives the indice of the ref determinant in psi_det.
  END_DOC
  integer                        :: i, k, l
  logical                        :: good
  double precision, parameter :: threshold=0.05d0
  N_det_ref = 0
  t = threshold * abs_psi_coef_max
  do i=1,N_det
    good = .False.
    do l = 1, N_states
     psi_ref_coef(i,l) = 0.d0
     good = good.or.(dabs(psi_coef(i,l)) > t)
    enddo
    if (good) then
      N_det_ref = N_det_ref+1
      do k=1,N_int
        psi_ref(k,1,N_det_ref) = psi_det(k,1,i)
        psi_ref(k,2,N_det_ref) = psi_det(k,2,i)
      enddo
      idx_ref(N_det_ref) = i
      do k=1,N_states
        psi_ref_coef(N_det_ref,k) = psi_coef(i,k)
      enddo
    endif
  enddo
  call write_int(output_determinants,N_det_ref, 'Number of determinants in the reference')

END_PROVIDER

