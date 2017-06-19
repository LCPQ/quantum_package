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
  N_det_selectors = N_det
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_selectors, (N_int,2,psi_selectors_size) ]
&BEGIN_PROVIDER [ double precision, psi_selectors_coef, (psi_selectors_size,N_states) ]
  implicit none
  BEGIN_DOC
  ! Determinants on which we apply <i|H|psi> for perturbation.
  END_DOC
  integer                        :: i, k, l, m
  logical                        :: good

  do i=1,N_det_generators
    do k=1,N_int
      psi_selectors(k,1,i) = psi_det_generators(k,1,i)
      psi_selectors(k,2,i) = psi_det_generators(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det_generators
      psi_selectors_coef(i,k) = psi_coef_generators(i,k)
    enddo
  enddo

  m=N_det_generators

  do i=1,N_det
    do l=1,n_cas_bitmask
      good = .True.
      do k=1,N_int
        good = good .and. (                                         &
            iand(not(cas_bitmask(k,1,l)), psi_det_sorted(k,1,i)) ==         &
            iand(not(cas_bitmask(k,1,l)), HF_bitmask(k,1)) .and. (   &
            iand(not(cas_bitmask(k,2,l)), psi_det_sorted(k,2,i)) ==         &
            iand(not(cas_bitmask(k,2,l)), HF_bitmask(k,2) )) )
      enddo
      if (good) then
        exit
      endif
    enddo
    if (.not.good) then
      m = m+1
      do k=1,N_int
        psi_selectors(k,1,m) = psi_det_sorted(k,1,i)
        psi_selectors(k,2,m) = psi_det_sorted(k,2,i)
      enddo
      psi_selectors_coef(m,:) = psi_coef_sorted(i,:)
    endif
  enddo
  if (N_det /= m) then
    print *,  N_det, m
    stop 'N_det /= m'
  endif
END_PROVIDER

BEGIN_PROVIDER [ double precision, psi_selectors_coef_transp, (N_states,psi_selectors_size) ]
  implicit none
  BEGIN_DOC
  ! Transposed psi_selectors
  END_DOC
  integer                        :: i,k

  do i=1,N_det_selectors
    do k=1,N_states
      psi_selectors_coef_transp(k,i) = psi_selectors_coef(i,k) 
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


