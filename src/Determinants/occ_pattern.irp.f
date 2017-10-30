use bitmasks
subroutine det_to_occ_pattern(d,o,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Transform a determinant to an occupation pattern
  END_DOC
  integer          ,intent(in)   :: Nint
  integer(bit_kind),intent(in)   :: d(Nint,2)
  integer(bit_kind),intent(out)  :: o(Nint,2)

  integer                        :: k

  do k=1,Nint
    o(k,1) = ieor(d(k,1),d(k,2))
    o(k,2) = iand(d(k,1),d(k,2))
  enddo
end

subroutine occ_pattern_to_dets_size(o,sze,n_alpha,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
!  Number of possible determinants for a given occ_pattern
  END_DOC
  integer          ,intent(in)   :: Nint, n_alpha
  integer(bit_kind),intent(in)   :: o(Nint,2)
  integer, intent(out)           :: sze
  integer                        :: amax,bmax,k
  double precision, external     :: binom_func

  amax = n_alpha
  bmax = 0
  do k=1,Nint
    bmax += popcnt( o(k,1) )
    amax -= popcnt( o(k,2) )
  enddo
  sze = int( min(binom_func(bmax, amax), 1.d8) )
  sze = 2*sze*sze + 16

end

subroutine occ_pattern_to_dets(o,d,sze,n_alpha,Nint)
  use bitmasks
  implicit none
  BEGIN_DOC
  ! Generate all possible determinants for a give occ_pattern
  END_DOC
  integer          ,intent(in)   :: Nint, n_alpha
  integer         ,intent(inout) :: sze
  integer(bit_kind),intent(in)   :: o(Nint,2)
  integer(bit_kind),intent(out)  :: d(Nint,2,sze)
  
  integer                        :: i, k, nt, na, nd, amax
  integer                        :: list_todo(2*n_alpha)
  integer                        :: list_a(2*n_alpha)

  amax = n_alpha
  do k=1,Nint
    amax -= popcnt( o(k,2) )
  enddo

  call bitstring_to_list(o(1,1), list_todo, nt, Nint)

  na = 0
  nd = 0
  d = 0
  call rec_occ_pattern_to_dets(list_todo,nt,list_a,na,d,nd,sze,amax,Nint)

  sze = nd
  
  integer :: ne(2), l
  l=0
  do i=1,nd
    ne(1) = 0
    ne(2) = 0
    l=l+1
    ! Doubly occupied orbitals
    do k=1,Nint
      d(k,1,l) = ior(d(k,1,i),o(k,2))
      d(k,2,l) = ior(d(k,2,i),o(k,2))
      ne(1) += popcnt(d(k,1,l))
      ne(2) += popcnt(d(k,2,l))
    enddo
    if ( (ne(1) /= elec_alpha_num).or.(ne(2) /= elec_beta_num) ) then
      l = l-1
    endif
  enddo
  sze = l

end

recursive subroutine  rec_occ_pattern_to_dets(list_todo,nt,list_a,na,d,nd,sze,amax,Nint)
  use bitmasks
  implicit none

  integer, intent(in)            :: nt, sze, amax, Nint,na
  integer,intent(inout)          :: list_todo(nt)
  integer, intent(inout)         :: list_a(na+1),nd
  integer(bit_kind),intent(inout) :: d(Nint,2,sze)

  if (na == amax) then
    nd += 1
    if (na > 0) then
      call list_to_bitstring( d(1,1,nd), list_a, na, Nint)
    endif
    if (nt > 0) then
      call list_to_bitstring( d(1,2,nd), list_todo, nt, Nint)
    endif
  else
    integer :: i, j, k
    integer, allocatable :: list_todo_tmp(:)
    allocate (list_todo_tmp(nt))
    do i=1,nt
      if (na > 0) then
        if (list_todo(i) < list_a(na)) then
          cycle
        endif
      endif
      list_a(na+1) = list_todo(i)
      k=1
      do j=1,nt
        if (i/=j) then
          list_todo_tmp(k) = list_todo(j)
          k += 1
        endif
      enddo
      call rec_occ_pattern_to_dets(list_todo_tmp,nt-1,list_a,na+1,d,nd,sze,amax,Nint)
    enddo
    deallocate(list_todo_tmp)
  endif

end

 BEGIN_PROVIDER [ integer(bit_kind), psi_occ_pattern, (N_int,2,psi_det_size) ]
&BEGIN_PROVIDER [ integer, N_occ_pattern ]
 implicit none
 BEGIN_DOC
  ! array of the occ_pattern present in the wf
  ! psi_occ_pattern(:,1,j) = jth occ_pattern of the wave function : represent all the single occupations
  ! psi_occ_pattern(:,2,j) = jth occ_pattern of the wave function : represent all the double occupations
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


 allocate ( iorder(N_det), duplicate(N_det), bit_tmp(N_det), tmp_array(N_int,2,N_det) )

 do i=1,N_det
   iorder(i) = i
   !$DIR FORCEINLINE
   bit_tmp(i) = occ_pattern_search_key(psi_occ_pattern(1,1,i),N_int)
 enddo
 call i8sort(bit_tmp,iorder,N_det)
 !DIR$ IVDEP
 do i=1,N_det
  do k=1,N_int
    tmp_array(k,1,i) = psi_occ_pattern(k,1,iorder(i))
    tmp_array(k,2,i) = psi_occ_pattern(k,2,iorder(i))
  enddo
  duplicate(i) = .False.
 enddo

 ! Find duplicates
 do i=1,N_det-1
  if (duplicate(i)) then
    cycle
  endif
  j = i+1
  do while (bit_tmp(j)==bit_tmp(i))
    if (duplicate(j)) then
      j+=1
      if (j>N_det) then
        exit
      endif
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

 ! Copy filtered result
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

!- Check
!  do i=1,N_occ_pattern
!   do j=i+1,N_occ_pattern
!     duplicate(1) = .True.
!     do k=1,N_int
!       if (psi_occ_pattern(k,1,i) /= psi_occ_pattern(k,1,j)) then
!         duplicate(1) = .False.
!         exit
!       endif
!       if (psi_occ_pattern(k,2,i) /= psi_occ_pattern(k,2,j)) then
!         duplicate(1) = .False.
!         exit
!       endif
!     enddo
!     if (duplicate(1)) then
!       call debug_det(psi_occ_pattern(1,1,i),N_int)
!       call debug_det(psi_occ_pattern(1,1,j),N_int)
!       stop 'DUPLICATE'
!     endif
!   enddo
!  enddo
!-
 deallocate(iorder,duplicate,bit_tmp,tmp_array)

END_PROVIDER 

subroutine make_s2_eigenfunction
  implicit none
  integer                        :: i,j,k
  integer                        :: smax, s
  integer(bit_kind), allocatable :: d(:,:,:), det_buffer(:,:,:)
  integer                        :: N_det_new, ithread, omp_get_thread_num
  integer, parameter             :: bufsze = 1000
  logical, external              :: is_in_wavefunction

  call write_int(6,N_occ_pattern,'Number of occupation patterns')

  !$OMP PARALLEL DEFAULT(NONE) &
  !$OMP  SHARED(N_occ_pattern, psi_occ_pattern, elec_alpha_num,N_int) &
  !$OMP  PRIVATE(s,ithread, d, det_buffer, smax, N_det_new,i,j,k)
  N_det_new = 0
  call occ_pattern_to_dets_size(psi_occ_pattern(1,1,1),s,elec_alpha_num,N_int)
  allocate (d(N_int,2,s), det_buffer(N_int,2,bufsze) )
  smax = s
  ithread=0
  !$ ithread = omp_get_thread_num()
  !$OMP DO 
  do i=1,N_occ_pattern
    call occ_pattern_to_dets_size(psi_occ_pattern(1,1,i),s,elec_alpha_num,N_int)
    s += 1
    if (s > smax) then
      deallocate(d)
      allocate ( d(N_int,2,s) )
      smax = s
    endif
    call occ_pattern_to_dets(psi_occ_pattern(1,1,i),d,s,elec_alpha_num,N_int)
    do j=1,s
      if (.not. is_in_wavefunction(d(1,1,j), N_int) ) then
        N_det_new += 1
        do k=1,N_int
          det_buffer(k,1,N_det_new) = d(k,1,j)
          det_buffer(k,2,N_det_new) = d(k,2,j)
        enddo
        if (N_det_new == bufsze) then
          call fill_H_apply_buffer_no_selection(bufsze,det_buffer,N_int,ithread)
          N_det_new = 0
        endif
      endif
    enddo
  enddo
  !$OMP END DO NOWAIT

  if (N_det_new > 0) then
    call fill_H_apply_buffer_no_selection(N_det_new,det_buffer,N_int,ithread)
  endif
  !$OMP BARRIER
  deallocate(d,det_buffer)
  !$OMP END PARALLEL

  call copy_H_apply_buffer_to_wf
  SOFT_TOUCH N_det psi_coef psi_det
  print *,  'Added determinants for S^2'
  call write_time(6)

end



