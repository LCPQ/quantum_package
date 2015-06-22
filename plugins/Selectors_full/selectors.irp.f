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
 do i=1,N_det
   norm = norm + psi_average_norm_contrib_sorted(i)
   if (norm > threshold_selectors) then
     N_det_selectors = i-1
     exit
   endif
 enddo
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


 BEGIN_PROVIDER [ integer(bit_kind), psi_selectors_ab, (N_int,2,psi_selectors_size) ]
&BEGIN_PROVIDER [ double precision, psi_selectors_coef_ab, (psi_selectors_size,N_states) ]
&BEGIN_PROVIDER [ integer, psi_selectors_next_ab, (2,psi_selectors_size) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which we apply <i|H|j>.
 ! They are sorted by the 3 highest electrons in the alpha part,
 ! then by the 3 highest electrons in the beta part to accelerate
 ! the research of connected determinants.
 END_DOC
 integer                        :: i,j,k
 integer, allocatable           :: iorder(:)
 integer*8, allocatable         :: bit_tmp(:)
 integer*8, external            :: det_search_key
 
 allocate ( iorder(N_det_selectors), bit_tmp(N_det_selectors) )
 
 ! Sort alpha dets
 ! ---------------
 
 integer(bit_kind)              :: det_tmp(N_int)

 do i=1,N_det_selectors
   iorder(i) = i
   call int_of_3_highest_electrons(psi_selectors(1,1,i),bit_tmp(i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,N_det_selectors)
 !DIR$ IVDEP
 do i=1,N_det_selectors
   do j=1,N_int
     psi_selectors_ab(j,1,i) = psi_selectors(j,1,iorder(i))
     psi_selectors_ab(j,2,i) = psi_selectors(j,2,iorder(i))
   enddo
   do k=1,N_states
     psi_coef_sorted_ab(i,k) = psi_selectors_coef(iorder(i),k)
   enddo
 enddo
 
 ! Find next alpha
 ! ---------------

 integer                        :: next

 next = N_det_selectors+1
 psi_selectors_next_ab(1,N_det_selectors) = next
 do i=N_det_selectors-1,1,-1
  if (bit_tmp(i) /= bit_tmp(i+1)) then
    next = i+1
  endif
  psi_selectors_next_ab(1,i) = next
 enddo

 ! Sort beta dets
 ! --------------

 integer :: istart, iend
 integer(bit_kind), allocatable :: psi_selectors_ab_temp (:,:) 

 allocate ( psi_selectors_ab_temp (N_int,N_det_selectors) )
 do i=1,N_det_selectors
   do j=1,N_int
     psi_selectors_ab_temp(j,i) = psi_selectors_ab(j,2,i)
   enddo
   iorder(i) = i
   call int_of_3_highest_electrons(psi_selectors_ab_temp(1,i),bit_tmp(i),N_int)
 enddo

 istart=1
 do while ( istart<N_det_selectors )
 
   iend = psi_selectors_next_ab(1,istart)
   call i8sort(bit_tmp(istart),iorder(istart),iend-istart)
   !DIR$ IVDEP
   do i=istart,iend-1
     do j=1,N_int
       psi_selectors_ab(j,2,i) = psi_selectors_ab_temp(j,iorder(i))
     enddo
     do k=1,N_states
       psi_coef_sorted_ab(i,k) = psi_coef(iorder(i),k)
     enddo
   enddo

   next = iend
   psi_selectors_next_ab(2,iend-1) = next
   do i=iend-2,1,-1
    if (bit_tmp(i) /= bit_tmp(i+1)) then
      next = i+1
    endif
    psi_selectors_next_ab(2,i) = next
   enddo

   istart = iend
 enddo

 deallocate(iorder, bit_tmp, psi_selectors_ab_temp)

END_PROVIDER
