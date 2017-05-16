
subroutine create_selection_buffer(N, siz, res)
  use selection_types
  implicit none

  integer, intent(in) :: N, siz
  type(selection_buffer), intent(out) :: res

  allocate(res%det(N_int, 2, siz), res%val(siz))

  res%val = 0d0
  res%det = 0_8
  res%N = N
  res%mini = 0d0
  res%cur = 0
end subroutine

subroutine delete_selection_buffer(b)
  use selection_types
  implicit none
  type(selection_buffer), intent(inout) :: b
  if (associated(b%det)) then
    deallocate(b%det)
  endif
  if (associated(b%val)) then
    deallocate(b%val)
  endif
end

  
subroutine add_to_selection_buffer(b, det, val)
  use selection_types
  implicit none

  type(selection_buffer), intent(inout) :: b
  integer(bit_kind), intent(in) :: det(N_int, 2)
  double precision, intent(in) :: val
  integer :: i

  if(val <= b%mini) then
    b%cur += 1
    b%det(1:N_int,1:2,b%cur) = det(1:N_int,1:2)
    b%val(b%cur) = val
    if(b%cur == size(b%val)) then
      call sort_selection_buffer(b)
    end if
  end if
end subroutine

subroutine merge_selection_buffers(b1, b2)
  use selection_types
  implicit none
  BEGIN_DOC
! Merges the selection buffers b1 and b2 into b2
  END_DOC
  type(selection_buffer), intent(inout) :: b1
  type(selection_buffer), intent(inout) :: b2
  integer(bit_kind), pointer     :: detmp(:,:,:)
  double precision, pointer      :: val(:)
  integer                        :: i, i1, i2, k, nmwen
  if (b1%cur == 0) return
  do while (b1%val(b1%cur) > b2%mini)
    b1%cur = b1%cur-1
    if (b1%cur == 0) then
      return
    endif
  enddo
  nmwen = min(b1%N, b1%cur+b2%cur)
  allocate( val(size(b1%val)), detmp(N_int, 2, size(b1%det,3)) )
  i1=1
  i2=1
  do i=1,nmwen
    if ( (i1 > b1%cur).and.(i2 > b2%cur) ) then
      exit 
    else if (i1 > b1%cur) then
        val(i) = b2%val(i2)
        detmp(1:N_int,1,i) = b2%det(1:N_int,1,i2)
        detmp(1:N_int,2,i) = b2%det(1:N_int,2,i2)
        i2=i2+1
    else if (i2 > b2%cur) then
        val(i) = b1%val(i1)
        detmp(1:N_int,1,i) = b1%det(1:N_int,1,i1)
        detmp(1:N_int,2,i) = b1%det(1:N_int,2,i1)
        i1=i1+1
    else
      if (b1%val(i1) <= b2%val(i2)) then
        val(i) = b1%val(i1)
        detmp(1:N_int,1,i) = b1%det(1:N_int,1,i1)
        detmp(1:N_int,2,i) = b1%det(1:N_int,2,i1)
        i1=i1+1
      else
        val(i) = b2%val(i2)
        detmp(1:N_int,1,i) = b2%det(1:N_int,1,i2)
        detmp(1:N_int,2,i) = b2%det(1:N_int,2,i2)
        i2=i2+1
      endif
    endif
  enddo
  deallocate(b2%det, b2%val)
  b2%det => detmp
  b2%val => val
  b2%mini = min(b2%mini,b2%val(b2%N))
  b2%cur = nmwen
end


subroutine sort_selection_buffer(b)
  use selection_types
  implicit none

  type(selection_buffer), intent(inout) :: b
  integer, allocatable :: iorder(:)
  integer(bit_kind), pointer :: detmp(:,:,:)
  integer :: i, nmwen
  logical, external :: detEq
  if (b%cur == 0) return
  nmwen = min(b%N, b%cur)

  allocate(iorder(b%cur), detmp(N_int, 2, size(b%det,3)))
  do i=1,b%cur
    iorder(i) = i
  end do
  call dsort(b%val, iorder, b%cur)
  do i=1, nmwen
    detmp(1:N_int,1,i) = b%det(1:N_int,1,iorder(i))
    detmp(1:N_int,2,i) = b%det(1:N_int,2,iorder(i))
  end do
  deallocate(b%det,iorder)
  b%det => detmp
  b%mini = min(b%mini,b%val(b%N))
  b%cur = nmwen
end subroutine

