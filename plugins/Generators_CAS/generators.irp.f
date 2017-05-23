use bitmasks

BEGIN_PROVIDER [ integer, N_det_generators ]
  implicit none
  BEGIN_DOC
  ! Number of generator detetrminants
  END_DOC
  integer                        :: i,k,l
  logical                        :: good
  call write_time(output_determinants)
  N_det_generators = 0
  do i=1,N_det
    do l=1,n_cas_bitmask
      good = .True.
      do k=1,N_int
        good = good .and. (                                          &
            iand(not(cas_bitmask(k,1,l)), psi_det_sorted(k,1,i)) ==         &
            iand(not(cas_bitmask(k,1,l)), HF_bitmask(k,1)) ) .and. ( &
            iand(not(cas_bitmask(k,2,l)), psi_det_sorted(k,2,i)) ==         &
            iand(not(cas_bitmask(k,2,l)), HF_bitmask(k,2)) )
      enddo
      if (good) then
        exit
      endif
    enddo
    if (good) then
      N_det_generators += 1
    endif
  enddo
  N_det_generators = max(N_det_generators,1)
  call write_int(output_determinants,N_det_generators,'Number of generators')
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators, (psi_det_size,N_states) ]
  implicit none
  BEGIN_DOC
  ! For Single reference wave functions, the generator is the
  ! Hartree-Fock determinant
  END_DOC
  integer                        :: i, k, l, m
  logical                        :: good
  m=0
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
    if (good) then
      m = m+1
      do k=1,N_int
        psi_det_generators(k,1,m) = psi_det_sorted(k,1,i)
        psi_det_generators(k,2,m) = psi_det_sorted(k,2,i)
      enddo
      psi_coef_generators(m,:) = psi_coef(m,:)
    endif
  enddo
  
END_PROVIDER

BEGIN_PROVIDER [ integer, size_select_max]
  implicit none
  BEGIN_DOC
  ! Size of the select_max array
  END_DOC
  size_select_max = 10000
END_PROVIDER

BEGIN_PROVIDER [ double precision, select_max, (size_select_max) ]
  implicit none
  BEGIN_DOC
  ! Memo to skip useless selectors
  END_DOC
  select_max = huge(1.d0)
END_PROVIDER

