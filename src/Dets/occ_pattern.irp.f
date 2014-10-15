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
  integer                        :: list_todo(n_alpha)
  integer                        :: list_a(n_alpha)

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
  
  do i=1,nd
    ! Doubly occupied orbitals
    do k=1,Nint
      d(k,1,i) = ior(d(k,1,i),o(k,2))
      d(k,2,i) = ior(d(k,2,i),o(k,2))
    enddo
  enddo

!  !TODO DEBUG
!  integer :: j,s
!  do i=1,nd
!    do j=1,i-1
!      na=0
!      do k=1,Nint
!        if((d(k,1,j) /= d(k,1,i)).or. &
!           (d(k,2,j) /= d(k,2,i))) then
!          s=1
!          exit
!        endif
!      enddo
!      if ( j== 0 ) then
!        print *,  'det ',i,' and ',j,' equal:'
!        call debug_det(d(1,1,j),Nint)
!        call debug_det(d(1,1,i),Nint)
!        stop
!      endif
!    enddo
!  enddo
!  !TODO DEBUG
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
    integer :: list_todo_tmp(nt)
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
  endif

end

