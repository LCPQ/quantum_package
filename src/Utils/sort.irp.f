BEGIN_TEMPLATE
 subroutine insertion_$Xsort (x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! Sort array x(isize) using the insertion sort algorithm.
  ! iorder in input should be (1,2,3,...,isize), and in output
  ! contains the new order of the elements.
  END_DOC
  $type,intent(inout)            :: x(isize)
  integer,intent(inout)          :: iorder(isize)
  integer,intent(in)             :: isize
  $type                          :: xtmp
  integer                        :: i, i0, j, jmax
  
  do i=1,isize
    xtmp = x(i)
    i0 = iorder(i)
    j = i-1
    do j=i-1,1,-1
      if ( x(j) > xtmp ) then
        x(j+1) = x(j)
        iorder(j+1) = iorder(j)
      else
        exit
      endif
    enddo
    x(j+1) = xtmp
    iorder(j+1) = i0
  enddo
 end subroutine insertion_$Xsort

 subroutine heap_$Xsort(x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! Sort array x(isize) using the heap sort algorithm.
  ! iorder in input should be (1,2,3,...,isize), and in output
  ! contains the new order of the elements.
  END_DOC
  $type,intent(inout)            :: x(isize)
  integer,intent(inout)          :: iorder(isize)
  integer,intent(in)             :: isize
  
  integer                        :: i, k, j, l, i0
  $type                          :: xtemp
  
  l = isize/2+1
  k = isize
  do while (.True.)
    if (l>1) then
      l=l-1
      xtemp = x(l)
      i0 = iorder(l)
    else
      xtemp = x(k)
      i0 = iorder(k)
      x(k) = x(1)
      iorder(k) = iorder(1)
      k = k-1
      if (k == 1) then
        x(1) = xtemp
        iorder(1) = i0
        exit
      endif
    endif
    i=l
    j = ishft(l,1)
    do while (j<k)
      if ( x(j) < x(j+1) ) then
        j=j+1
      endif
      if (xtemp < x(j)) then
        x(i) = x(j)
        iorder(i) = iorder(j)
        i = j
        j = ishft(j,1)
      else
        j = k+1
      endif
    enddo
    if (j==k) then
      if (xtemp < x(j)) then
        x(i) = x(j)
        iorder(i) = iorder(j)
        i = j
        j = ishft(j,1)
      else
        j = k+1
      endif
    endif
    x(i) = xtemp
    iorder(i) = i0
  enddo
 end subroutine heap_$Xsort

 subroutine heap_$Xsort_big(x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! Sort array x(isize) using the heap sort algorithm.
  ! iorder in input should be (1,2,3,...,isize), and in output
  ! contains the new order of the elements.
  ! This is a version for very large arrays where the indices need
  ! to be in integer*8 format
  END_DOC
  $type,intent(inout)            :: x(isize)
  integer*8,intent(inout)        :: iorder(isize)
  integer*8,intent(in)           :: isize
  
  integer*8                      :: i, k, j, l, i0
  $type                          :: xtemp
  
  l = isize/2+1
  k = isize
  do while (.True.)
    if (l>1) then
      l=l-1
      xtemp = x(l)
      i0 = iorder(l)
    else
      xtemp = x(k)
      i0 = iorder(k)
      x(k) = x(1)
      iorder(k) = iorder(1)
      k = k-1
      if (k == 1) then
        x(1) = xtemp
        iorder(1) = i0
        exit
      endif
    endif
    i=l
    j = ishft(l,1)
    do while (j<k)
      if ( x(j) < x(j+1) ) then
        j=j+1
      endif
      if (xtemp < x(j)) then
        x(i) = x(j)
        iorder(i) = iorder(j)
        i = j
        j = ishft(j,1)
      else
        j = k+1
      endif
    enddo
    if (j==k) then
      if (xtemp < x(j)) then
        x(i) = x(j)
        iorder(i) = iorder(j)
        i = j
        j = ishft(j,1)
      else
        j = k+1
      endif
    endif
    x(i) = xtemp
    iorder(i) = i0
  enddo
  
 end subroutine heap_$Xsort$big

 subroutine $Xsort(x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! Sort array x(isize).
  ! iorder in input should be (1,2,3,...,isize), and in output
  ! contains the new order of the elements.
  END_DOC
  $type,intent(inout)            :: x(isize)
  integer,intent(inout)          :: iorder(isize)
  integer,intent(in)             :: isize
  if (isize < 32) then
    call insertion_$Xsort(x,iorder,isize)
  else
    call heap_$Xsort(x,iorder,isize)
  endif
 end subroutine $Xsort

SUBST [ X, type ]
   ; real ;;
 d ; double precision ;;
 i ; integer ;;
 i8 ; integer*8 ;;
 i2 ; integer*2 ;;
END_TEMPLATE

BEGIN_TEMPLATE
 subroutine $Xset_order(x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! array A has already been sorted, and iorder has contains the new order of
  ! elements of A. This subroutine changes the order of x to match the new order of A.
  END_DOC
  integer                        :: isize
  $type                          :: x(*)
  $type,allocatable              :: xtmp(:)
  integer                        :: iorder(*)
  integer                        :: i
  
  allocate(xtmp(isize))
  do i=1,isize
    xtmp(i) = x(iorder(i))
  enddo
  
  do i=1,isize
    x(i) = xtmp(i)
  enddo
  deallocate(xtmp)
 end

SUBST [ X, type ]
   ; real ;;
 d ; double precision ;;
 i ; integer ;;
 i8; integer*8 ;;
 i2; integer*2 ;;
END_TEMPLATE


BEGIN_TEMPLATE
 subroutine insertion_$Xsort_big (x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! Sort array x(isize) using the insertion sort algorithm.
  ! iorder in input should be (1,2,3,...,isize), and in output
  ! contains the new order of the elements.
  ! This is a version for very large arrays where the indices need
  ! to be in integer*8 format
  END_DOC
  $type,intent(inout)            :: x(isize)
  integer*8,intent(inout)        :: iorder(isize)
  integer*8,intent(in)           :: isize
  $type                          :: xtmp
  integer*8                      :: i, i0, j, jmax
  
  do i=1_8,isize
    xtmp = x(i)
    i0 = iorder(i)
    j = i-1_8
    do j=i-1_8,1_8,-1_8
      if ( x(j) > xtmp ) then
        x(j+1_8) = x(j)
        iorder(j+1_8) = iorder(j)
      else
        exit
      endif
    enddo
    x(j+1_8) = xtmp
    iorder(j+1_8) = i0
  enddo
  
 end subroutine insertion_$Xsort

 subroutine $Xset_order_big(x,iorder,isize)
  implicit none
  BEGIN_DOC
  ! array A has already been sorted, and iorder has contains the new order of
  ! elements of A. This subroutine changes the order of x to match the new order of A.
  ! This is a version for very large arrays where the indices need
  ! to be in integer*8 format
  END_DOC
  integer*8                      :: isize
  $type                          :: x(*)
  $type, allocatable             :: xtmp(:)
  integer*8                      :: iorder(*)
  integer*8                      :: i
  allocate(xtmp(isize))
  do i=1_8,isize
    xtmp(i) = x(iorder(i))
  enddo
  
  do i=1_8,isize
    x(i) = xtmp(i)
  enddo
  deallocate(xtmp)
 end

SUBST [ X, type ]
   ; real ;;
 d ; double precision ;;
 i ; integer ;;
 i8; integer*8 ;;
 i2; integer*2 ;;
END_TEMPLATE

BEGIN_TEMPLATE 

 recursive subroutine $Xradix_sort$big(x,iorder,isize,iradix)
  implicit none
  BEGIN_DOC
  ! Sort integer array x(isize) using the radix sort algorithm.
  ! iorder in input should be (1,2,3,...,isize), and in output
  ! contains the new order of the elements.
  ! iradix should be -1 in input.
  END_DOC
  $int_type, intent(in)          :: isize
  $int_type, intent(inout)       :: iorder(isize)
  $type, intent(inout)           :: x(isize)
  integer, intent(in)            :: iradix
  integer                        :: iradix_new
  $type, allocatable             :: x2(:), x1(:)
  $int_type, allocatable         :: iorder1(:),iorder2(:)
  $int_type                      :: i0, i1, i2, i3, i
  integer, parameter             :: integer_size=$octets
  $type, parameter               :: zero=$zero
  $type                          :: mask
  integer                        :: nthreads, omp_get_num_threads
  !DIR$ ATTRIBUTES ALIGN : 128   :: iorder1,iorder2, x2, x1
  
  if (iradix == -1) then
    
    ! Find most significant bit
    
    i0 = 0_8
    i3 = -1_8
    
    do i=1,isize
      i3 = max(i3,x(i))
    enddo
    
    iradix_new = integer_size-1-leadz(i3)
    mask = ibset(zero,iradix_new)
    nthreads = 1
    !   nthreads = 1+ishft(omp_get_num_threads(),-1)
    
    integer                        :: err
    allocate(x1(isize/nthreads+1),iorder1(isize/nthreads+1),x2(isize/nthreads+1),iorder2(isize/nthreads+1),stat=err)
    if (err /= 0) then
      print *,  irp_here, ': Unable to allocate arrays'
      stop
    endif
    
    i1=1_8
    i2=1_8
    
    do i=1,isize
      if (iand(mask,x(i)) == zero) then
        iorder1(i1) = iorder(i)
        x1(i1) = x(i)
        i1 = i1+1_8
      else
        iorder2(i2) = iorder(i)
        x2(i2) = x(i)
        i2 = i2+1_8
      endif
    enddo
    i1=i1-1_8
    i2=i2-1_8
    
    do i=1,i1
      iorder(i0+i) = iorder1(i)
      x(i0+i) = x1(i)
    enddo
    i0 = i0+i1
    i3 = i0
    deallocate(x1,iorder1,stat=err)
    if (err /= 0) then
      print *,  irp_here, ': Unable to deallocate arrays x1, iorder1'
      stop
    endif
    
    
    do i=1,i2
      iorder(i0+i) = iorder2(i)
      x(i0+i) = x2(i)
    enddo
    i0 = i0+i2
    deallocate(x2,iorder2,stat=err)
    if (err /= 0) then
      print *,  irp_here, ': Unable to deallocate arrays x2, iorder2'
      stop
    endif
    
    
    if (i3>1) then
      call $Xradix_sort$big(x,iorder,i3,iradix_new-1)
    endif
    
    if (isize-i3>1) then
      call $Xradix_sort$big(x(i3+1),iorder(i3+1),isize-i3,iradix_new-1)
    endif
    
    return
  endif
  
  ASSERT (iradix >= 0)

  if (isize < 48) then
    call insertion_$Xsort$big(x,iorder,isize)
    return
  endif
  
  
  allocate(x2(isize),iorder2(isize),stat=err)
  if (err /= 0) then
    print *,  irp_here, ': Unable to allocate arrays x1, iorder1'
    stop
  endif
  
  
  mask = ibset(zero,iradix)
  i0=1
  i1=1
  
  do i=1,isize
    if (iand(mask,x(i)) == zero) then
      iorder(i0) = iorder(i)
      x(i0) = x(i)
      i0 = i0+1
    else
      iorder2(i1) = iorder(i)
      x2(i1) = x(i)
      i1 = i1+1
    endif
  enddo
  i0=i0-1
  i1=i1-1
  
  do i=1,i1
    iorder(i0+i) = iorder2(i)
    x(i0+i) = x2(i)
  enddo
  
  deallocate(x2,iorder2,stat=err)
  if (err /= 0) then
    print *,  irp_here, ': Unable to allocate arrays x2, iorder2'
    stop
  endif
  
  
  if (iradix == 0) then
    return
  endif
  
  
  if (i1>1) then
    call $Xradix_sort$big(x(i0+1),iorder(i0+1),i1,iradix-1)
  endif
  if (i0>1) then
    call $Xradix_sort$big(x,iorder,i0,iradix-1)
  endif
  
 end

SUBST [ X, type, octets, is_big, big, int_type, zero ]
 i  ; integer   ; 32 ; .False. ; ; integer ; 0;;
 i8 ; integer*8 ; 32 ; .False. ; ; integer ; 0_8;;
 i2 ; integer*2 ; 32 ; .False. ; ; integer ; 0;;
 i  ; integer   ; 64 ; .True. ; _big ; integer*8 ; 0 ;;
 i8 ; integer*8 ; 64 ; .True. ; _big ; integer*8 ; 0_8 ;;
END_TEMPLATE


