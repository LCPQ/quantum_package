use bitmasks

BEGIN_PROVIDER [ integer, N_det ]
 implicit none
 BEGIN_DOC
 ! Number of determinants in the wave function
 END_DOC
 logical                        :: exists
 character*64                   :: label
 PROVIDE ezfio_filename
 if (read_wf) then
   call ezfio_has_determinants_n_det(exists)
   if (exists) then
       call ezfio_has_determinants_mo_label(exists)
       if (exists) then
         call ezfio_get_determinants_mo_label(label)
         exists = (label == mo_label)
       endif
   endif
   if (exists) then
     call ezfio_get_determinants_n_det(N_det)
   else
     N_det = 1
   endif
 else
   N_det = 1
 endif
 call write_int(output_dets,N_det,'Number of determinants')
 ASSERT (N_det > 0)
END_PROVIDER


BEGIN_PROVIDER [ integer, psi_det_size ]
 implicit none
 BEGIN_DOC
 ! Size of the psi_det/psi_coef arrays
 END_DOC
 PROVIDE ezfio_filename
 logical                        :: exists
 call ezfio_has_determinants_n_det(exists)
 if (exists) then
   call ezfio_get_determinants_n_det(psi_det_size)
 else
   psi_det_size = 1
 endif
 psi_det_size = max(psi_det_size,10000)
 call write_int(output_dets,psi_det_size,'Dimension of the psi arrays')

END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_det, (N_int,2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! The wave function determinants. Initialized with Hartree-Fock if the EZFIO file
 ! is empty
 END_DOC
  integer                        :: i
  logical                        :: exists
  character*64                   :: label
  
  if (read_wf) then
    call ezfio_has_determinants_N_int(exists)
    if (exists) then
     call ezfio_has_determinants_bit_kind(exists)
     if (exists) then
      call ezfio_has_determinants_N_det(exists)
      if (exists) then
       call ezfio_has_determinants_N_states(exists)
       if (exists) then
        call ezfio_has_determinants_psi_det(exists)
        if (exists) then
          call ezfio_has_determinants_mo_label(exists)
          if (exists) then
            call ezfio_get_determinants_mo_label(label)
            exists = (label == mo_label)
          endif
        endif
       endif
      endif
     endif
    endif
   
    if (exists) then
      call read_dets(psi_det,N_int,N_det)
    else
       psi_det = 0_bit_kind
       do i=1,N_int
         psi_det(i,1,1) = HF_bitmask(i,1)
         psi_det(i,2,1) = HF_bitmask(i,2)
       enddo
    endif
  else
     psi_det = 0_bit_kind
     do i=1,N_int
       psi_det(i,1,1) = HF_bitmask(i,1)
       psi_det(i,2,1) = HF_bitmask(i,2)
     enddo
  endif

END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), psi_occ_pattern, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_occ_pattern ]
 implicit none
 BEGIN_DOC
  ! array of the occ_pattern present in the wf
  ! psi_occ_pattern(:,1,j) = jth occ_pattern of the wave function : represent all the single occupation
  ! psi_occ_pattern(:,2,j) = jth occ_pattern of the wave function : represent all the double occupation
 END_DOC
 integer :: i,j,k

 ! create
 do i = 1, N_det
  do k = 1, N_int
   psi_occ_pattern(k,1,i) = ieor(psi_det(k,1,i),psi_det(k,2,i))
   psi_occ_pattern(k,2,i) = iand(psi_det(k,1,i),psi_det(k,2,i))
  enddo
 enddo

 ! Sort
 integer, allocatable           :: iorder(:)
 integer*8, allocatable         :: bit_tmp(:)
 integer*8, external            :: occ_pattern_search_key
 integer(bit_kind), allocatable :: tmp_array(:,:,:)
 logical,allocatable            :: duplicate(:)


 allocate ( iorder(N_det), duplicate(N_det), bit_tmp(N_det), tmp_array(N_int,2,psi_det_size) )

 do i=1,N_det
   iorder(i) = i
   !$DIR FORCEINLINE
   bit_tmp(i) = occ_pattern_search_key(psi_occ_pattern(1,1,i),N_int)
 enddo
 print*,'passed 1'
 call i8sort(bit_tmp,iorder,N_det)
 print*,'passed 2'
 !DIR$ IVDEP
 do i=1,N_det
  do k=1,N_int
    tmp_array(k,1,i) = psi_occ_pattern(k,1,iorder(i))
    tmp_array(k,2,i) = psi_occ_pattern(k,2,iorder(i))
  enddo
  duplicate(i) = .False.
 enddo

 i=1
 integer (bit_kind) :: occ_pattern_tmp
 do i=1,N_det
  duplicate(i) = .False.
 enddo

 do i=1,N_det-1
  if (duplicate(i)) then
    cycle
  endif
  j = i+1
  do while (bit_tmp(j)==bit_tmp(i))
    if (duplicate(j)) then
      j+=1
      cycle
    endif
    duplicate(j) = .True.
    do k=1,N_int
      if ( (tmp_array(k,1,i) /= tmp_array(k,1,j)) &
      .or. (tmp_array(k,2,i) /= tmp_array(k,2,j)) ) then
         duplicate(j) = .False.
         exit
      endif
    enddo
    j+=1
    if (j>N_det) then
      exit
    endif
  enddo
 enddo
  print*,'passed 3'

 N_occ_pattern=0
 do i=1,N_det
  if (duplicate(i)) then
    cycle
  endif
  N_occ_pattern += 1
  do k=1,N_int
    psi_occ_pattern(k,1,N_occ_pattern) = tmp_array(k,1,i)
    psi_occ_pattern(k,2,N_occ_pattern) = tmp_array(k,2,i)
  enddo
 enddo

 deallocate(iorder,duplicate,bit_tmp,tmp_array)
! !TODO DEBUG
! integer :: s
! do i=1,N_occ_pattern
!   do j=i+1,N_occ_pattern
!    s = 0
!    do k=1,N_int
!      if((psi_occ_pattern(k,1,j) /= psi_occ_pattern(k,1,i)).or. &
!         (psi_occ_pattern(k,2,j) /= psi_occ_pattern(k,2,i))) then
!         s=1
!         exit
!      endif
!    enddo
!    if ( s == 0 ) then
!      print *,  'Error : occ ', j, 'already in wf'
!      call debug_det(psi_occ_pattern(1,1,j),N_int)
!      stop
!    endif
!   enddo
! enddo
! !TODO DEBUG
END_PROVIDER 


BEGIN_PROVIDER [ double precision, psi_coef, (psi_det_size,N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! The wave function coefficients. Initialized with Hartree-Fock if the EZFIO file
  ! is empty
  END_DOC
  
  integer                        :: i,k, N_int2
  logical                        :: exists
  double precision, allocatable  :: psi_coef_read(:,:)
  character*(64)                 :: label

  psi_coef = 0.d0
  do i=1,N_states_diag
    psi_coef(i,i) = 1.d0
  enddo
  
  if (read_wf) then
    call ezfio_has_determinants_psi_coef(exists)
    if (exists) then
      call ezfio_has_determinants_mo_label(exists)
      if (exists) then
        call ezfio_get_determinants_mo_label(label)
        exists = (label == mo_label)
      endif
    endif
    
    if (exists) then
      
      allocate (psi_coef_read(N_det,N_states))
      call ezfio_get_determinants_psi_coef(psi_coef_read)
      do k=1,N_states
        do i=1,N_det
          psi_coef(i,k) = psi_coef_read(i,k)
        enddo
      enddo
      deallocate(psi_coef_read)
      
    endif
    
  endif
    
  
END_PROVIDER


BEGIN_PROVIDER [ double precision, psi_average_norm_contrib, (psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Contribution of determinants to the state-averaged density
 END_DOC
 integer :: i,j,k
 double precision :: f
 f = 1.d0/dble(N_states)
 do i=1,N_det
   psi_average_norm_contrib(i) = psi_coef(i,1)*psi_coef(i,1)*f
 enddo
 do k=2,N_states
   do i=1,N_det
     psi_average_norm_contrib(i) = psi_average_norm_contrib(i) + &
       psi_coef(i,k)*psi_coef(i,k)*f
   enddo
 enddo
END_PROVIDER



!==============================================================================!
!                                                                              !
!                          Independent alpha/beta parts                        !
!                                                                              !
!==============================================================================!

integer*8 function spin_det_search_key(det,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
! Return an integer*8 corresponding to a determinant index for searching
  END_DOC
  integer, intent(in) :: Nint
  integer(bit_kind), intent(in) :: det(Nint)
  integer :: i
  spin_det_search_key = det(1)
  do i=2,Nint
    spin_det_search_key = ieor(spin_det_search_key,det(i))
  enddo
end


BEGIN_PROVIDER [ integer(bit_kind), psi_det_alpha, (N_int,psi_det_size) ]
 implicit none
 BEGIN_DOC
! List of alpha determinants of psi_det
 END_DOC
 integer :: i,k

 do i=1,N_det
   do k=1,N_int
     psi_det_alpha(k,i) = psi_det(k,1,i)
   enddo
 enddo
END_PROVIDER

BEGIN_PROVIDER [ integer(bit_kind), psi_det_beta, (N_int,psi_det_size) ]
 implicit none
 BEGIN_DOC
! List of beta determinants of psi_det
 END_DOC
 integer :: i,k

 do i=1,N_det
   do k=1,N_int
     psi_det_beta(k,i) = psi_det(k,2,i)
   enddo
 enddo
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_alpha_unique, (N_int,psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_det_alpha_unique ]
 implicit none
 BEGIN_DOC
 ! Unique alpha determinants
 END_DOC

 integer                        :: i,k
 integer, allocatable           :: iorder(:)
 integer*8, allocatable         :: bit_tmp(:)
 integer*8                      :: last_key
 integer*8, external            :: spin_det_search_key

 allocate ( iorder(N_det), bit_tmp(N_det))

 do i=1,N_det
   iorder(i) = i
   bit_tmp(i) = spin_det_search_key(psi_det_alpha(1,i),N_int)
 enddo

 call i8sort(bit_tmp,iorder,N_det)

 N_det_alpha_unique = 0
 last_key = 0_8
 do i=1,N_det
  if (bit_tmp(i) /= last_key) then
    last_key = bit_tmp(i)
    N_det_alpha_unique += 1
    do k=1,N_int
      psi_det_alpha_unique(k,N_det_alpha_unique) = psi_det_alpha(k,iorder(i))
    enddo
  endif
 enddo

 deallocate (iorder, bit_tmp)
END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_beta_unique, (N_int,psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_det_beta_unique ]
 implicit none
 BEGIN_DOC
 ! Unique beta determinants
 END_DOC

 integer                        :: i,k
 integer, allocatable           :: iorder(:)
 integer*8, allocatable         :: bit_tmp(:)
 integer*8                      :: last_key
 integer*8, external            :: spin_det_search_key

 allocate ( iorder(N_det), bit_tmp(N_det))

 do i=1,N_det
   iorder(i) = i
   bit_tmp(i) = spin_det_search_key(psi_det_beta(1,i),N_int)
 enddo

 call i8sort(bit_tmp,iorder,N_det)

 N_det_beta_unique = 0
 last_key = 0_8
 do i=1,N_det
  if (bit_tmp(i) /= last_key) then
    last_key = bit_tmp(i)
    N_det_beta_unique += 1
    do k=1,N_int
      psi_det_beta_unique(k,N_det_beta_unique) = psi_det_beta(k,iorder(i))
    enddo
  endif
 enddo

 deallocate (iorder, bit_tmp)
END_PROVIDER


!==============================================================================!
!                                                                              !
!                               Sorting providers                              !
!                                                                              !
!==============================================================================!


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted, (psi_det_size,N_states) ]
&BEGIN_PROVIDER [ double precision, psi_average_norm_contrib_sorted, (psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Wave function sorted by determinants contribution to the norm (state-averaged)
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 allocate ( iorder(N_det) )
 do i=1,N_det
   psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib(i)
   iorder(i) = i
 enddo
 call dsort(psi_average_norm_contrib_sorted,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do j=1,N_int
    psi_det_sorted(j,1,i) = psi_det(j,1,iorder(i))
    psi_det_sorted(j,2,i) = psi_det(j,2,iorder(i))
  enddo
  do k=1,N_states
    psi_coef_sorted(i,k) = psi_coef(iorder(i),k)
  enddo
  psi_average_norm_contrib_sorted(i) = -psi_average_norm_contrib_sorted(i)
 enddo

 deallocate(iorder)

END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted_bit, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted_bit, (psi_det_size,N_states) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which we apply <i|H|psi> for perturbation.
 ! They are sorted by determinants interpreted as integers. Useful
 ! to accelerate the search of a random determinant in the wave
 ! function.
 END_DOC
 integer :: i,j,k
 integer, allocatable ::  iorder(:)
 integer*8, allocatable :: bit_tmp(:)
 integer*8, external :: det_search_key

 allocate ( iorder(N_det), bit_tmp(N_det) )

 do i=1,N_det
   iorder(i) = i
   !$DIR FORCEINLINE
   bit_tmp(i) = det_search_key(psi_det(1,1,i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do j=1,N_int
    psi_det_sorted_bit(j,1,i) = psi_det(j,1,iorder(i))
    psi_det_sorted_bit(j,2,i) = psi_det(j,2,iorder(i))
  enddo
  do k=1,N_states
    psi_coef_sorted_bit(i,k) = psi_coef(iorder(i),k)
  enddo
 enddo

 deallocate(iorder, bit_tmp)

END_PROVIDER


subroutine int_of_3_highest_electrons( det_in, res, Nint )
  implicit none
  use bitmasks
  integer,intent(in)             :: Nint
  integer(bit_kind)              :: det_in(Nint)
  integer*8                      :: res
  BEGIN_DOC
! Returns an integer*8 as :
!
! |_<--- 21 bits ---><--- 21 bits ---><--- 21 bits --->|
!
! |0<---   i1    ---><---   i2    ---><---   i3    --->| 
!
! It encodes the value of the indices of the 3 highest MOs
! in descending order
!
  END_DOC
  integer                        :: i, k, icount
  integer(bit_kind)              :: ix
  res = 0_8
  icount = 3
  do k=Nint,1,-1
    ix = det_in(k)
    do while (ix /= 0_bit_kind)
      i = bit_kind_size-1-leadz(ix)
      ix = ibclr(ix,i)
      res = ior(ishft(res, 21), i+ishft(k-1,bit_kind_shift))
      icount -= 1
      if (icount == 0) then
        return
      endif
    enddo
  enddo
end

subroutine filter_3_highest_electrons( det_in, det_out, Nint )
  implicit none
  use bitmasks
  integer,intent(in)             :: Nint
  integer(bit_kind)              :: det_in(Nint), det_out(Nint)
  BEGIN_DOC
! Returns a determinant with only the 3 highest electrons
  END_DOC
  integer                        :: i, k, icount
  integer(bit_kind)              :: ix
  det_out = 0_8
  icount = 3
  do k=Nint,1,-1
    ix = det_in(k)
    do while (ix /= 0_bit_kind)
      i = bit_kind_size-1-leadz(ix)
      ix = ibclr(ix,i)
      det_out(k) = ibset(det_out(k),i)
      icount -= 1
      if (icount == 0) then
        return
      endif
    enddo
  enddo
end
      
 BEGIN_PROVIDER [ integer(bit_kind), psi_det_sorted_ab, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ double precision, psi_coef_sorted_ab, (N_det,N_states) ]
&BEGIN_PROVIDER [ integer, psi_det_sorted_next_ab, (2,psi_det_size) ]
 implicit none
 BEGIN_DOC
 ! Determinants on which we apply <i|H|j>.
 ! They are sorted by the 3 highest electrons in the alpha part,
 ! then by the 3 highest electrons in the beta part to accelerate
 ! the research of connected determinants.
 END_DOC
 
 call sort_dets_by_3_highest_electrons(                              &
     psi_det,                                                        &
     psi_coef,                                                       &
     psi_det_sorted_ab,                                              &
     psi_coef_sorted_ab,                                             &
     psi_det_sorted_next_ab,                                         &
     N_det, N_states, N_int,                                         &
     psi_det_size                     )

END_PROVIDER

subroutine sort_dets_by_3_highest_electrons(det_in,coef_in,det_out,coef_out, &
  det_next, Ndet, Nstates, Nint, LDA)
 implicit none
 integer, intent(in)            :: Ndet, Nstates, Nint, LDA
 integer(bit_kind), intent(in)  :: det_in   (Nint,2,Ndet)
 integer(bit_kind), intent(out) :: det_out  (Nint,2,Ndet)
 integer, intent(out)           :: det_next (2,Ndet)
 double precision, intent(in)   :: coef_in  (LDA,Nstates)
 double precision, intent(out)  :: coef_out (LDA,Nstates)
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
 
 allocate ( iorder(Ndet), bit_tmp(Ndet) )
 
 ! Sort alpha dets
 ! ---------------
 
 integer(bit_kind)              :: det_tmp(Nint)

 do i=1,Ndet
   iorder(i) = i
   call int_of_3_highest_electrons(psi_det(1,1,i),bit_tmp(i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,Ndet)
 !DIR$ IVDEP
 do i=1,Ndet
   do j=1,N_int
     det_out(j,1,i) = psi_det(j,1,iorder(i))
     det_out(j,2,i) = psi_det(j,2,iorder(i))
   enddo
   do k=1,Nstates
     coef_out(i,k) = psi_coef(iorder(i),k)
   enddo
 enddo
 
 ! Find next alpha
 ! ---------------

 integer                        :: next

 next = Ndet+1
 det_next(1,Ndet) = next
 do i=Ndet-1,1,-1
  if (bit_tmp(i) /= bit_tmp(i+1)) then
    next = i+1
  endif
  det_next(1,i) = next
 enddo

 ! Sort beta dets
 ! --------------

 integer :: istart, iend
 integer(bit_kind), allocatable :: det_sorted_temp (:,:) 

 allocate ( det_sorted_temp (N_int,Ndet) )
 do i=1,Ndet
   do j=1,N_int
     det_sorted_temp(j,i) = det_out(j,2,i)
   enddo
   iorder(i) = i
   call int_of_3_highest_electrons(det_sorted_temp(1,i),bit_tmp(i),N_int)
 enddo

 istart=1
 do while ( istart<Ndet )
 
   iend = det_next(1,istart)
   call i8sort(bit_tmp(istart),iorder(istart),iend-istart)
   !DIR$ IVDEP
   do i=istart,iend-1
     do j=1,N_int
       det_out(j,2,i) = det_sorted_temp(j,iorder(i))
     enddo
     do k=1,Nstates
       coef_out(i,k) = psi_coef(iorder(i),k)
     enddo
   enddo

   next = iend
   det_next(2,iend-1) = next
   do i=iend-2,1,-1
    if (bit_tmp(i) /= bit_tmp(i+1)) then
      next = i+1
    endif
    det_next(2,i) = next
   enddo

   istart = iend
 enddo

 deallocate(iorder, bit_tmp, det_sorted_temp)

end

!==============================================================================!
!                                                                              !
!                             Read/write routines                              !
!                                                                              !
!==============================================================================!

subroutine read_dets(det,Nint,Ndet)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Reads the determinants from the EZFIO file
  END_DOC
  
  integer, intent(in)            :: Nint,Ndet
  integer(bit_kind), intent(out) :: det(Nint,2,Ndet)
  integer*8, allocatable         :: psi_det_read(:,:,:)
  double precision, allocatable  :: psi_coef_read(:,:)
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  integer                        :: N_int2
  integer                        :: i,k
  equivalence (det_8, det_bk)
  
  call ezfio_get_determinants_N_int(N_int2)
  ASSERT (N_int2 == Nint)
  call ezfio_get_determinants_bit_kind(k)
  ASSERT (k == bit_kind)
  
  N_int2 = (Nint*bit_kind)/8
  allocate (psi_det_read(N_int2,2,Ndet))
  call ezfio_get_determinants_psi_det (psi_det_read)
! print*,'N_int2 = ',N_int2,N_int
! print*,'k',k,bit_kind
! print*,'psi_det_read = ',Ndet
  do i=1,Ndet
    do k=1,N_int2
      det_8(k) = psi_det_read(k,1,i)
    enddo
    do k=1,Nint
      det(k,1,i) = det_bk(k)
    enddo
    do k=1,N_int2
      det_8(k) = psi_det_read(k,2,i)
    enddo
    do k=1,Nint
      det(k,2,i) = det_bk(k)
    enddo
  enddo
  deallocate(psi_det_read)
  
end


subroutine save_wavefunction
  implicit none
  use bitmasks
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  call save_wavefunction_general(N_det,N_states,psi_det_sorted,psi_coef_sorted)
end

subroutine save_wavefunction_general(ndet,nstates,psidet,psicoef)
  implicit none
  BEGIN_DOC
!  Save the wave function into the EZFIO file
  END_DOC
  use bitmasks
  integer, intent(in) :: ndet,nstates
  integer(bit_kind), intent(in) :: psidet(N_int,2,ndet)
  double precision, intent(in)  :: psicoef(ndet,nstates)
  integer*8, allocatable         :: psi_det_save(:,:,:)
  double precision, allocatable  :: psi_coef_save(:,:)
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  integer                        :: N_int2
  equivalence (det_8, det_bk)

  integer :: i,k

  PROVIDE progress_bar
  call start_progress(7,'Saving wfunction',0.d0)

  progress_bar(1) = 1
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_N_int(N_int)
  progress_bar(1) = 2
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_bit_kind(bit_kind)
  progress_bar(1) = 3
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_N_det(ndet)
  progress_bar(1) = 4
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_n_states(nstates)
  progress_bar(1) = 5
  progress_value = dble(progress_bar(1))
  call ezfio_set_determinants_mo_label(mo_label)

  progress_bar(1) = 6
  progress_value = dble(progress_bar(1))

  N_int2 = (N_int*bit_kind)/8
  allocate (psi_det_save(N_int2,2,ndet))
  do i=1,ndet
    do k=1,N_int
      det_bk(k) = psidet(k,1,i)
    enddo
    do k=1,N_int2
      psi_det_save(k,1,i) = det_8(k)
    enddo
    do k=1,N_int
      det_bk(k) = psidet(k,2,i)
    enddo
    do k=1,N_int2
      psi_det_save(k,2,i) = det_8(k)
    enddo
!   print*,psi_det_save
  enddo
  call ezfio_set_determinants_psi_det(psi_det_save)
  deallocate (psi_det_save)

  progress_bar(1) = 7
  progress_value = dble(progress_bar(1))
  allocate (psi_coef_save(ndet,nstates))
  do k=1,nstates
    do i=1,ndet
      psi_coef_save(i,k) = psicoef(i,k)
    enddo
  enddo
  call ezfio_set_determinants_psi_coef(psi_coef_save)
  call write_int(output_dets,ndet,'Saved determinants')
  call stop_progress
  deallocate (psi_coef_save)
end



