
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
  if (allocated(b%det)) then
    deallocate(b%det)
  endif
  if (allocated(b%val)) then
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

  if(dabs(val) >= b%mini) then
    b%cur += 1
    b%det(:,:,b%cur) = det(:,:)
    b%val(b%cur) = val
    if(b%cur == size(b%val)) then
      call sort_selection_buffer(b)
    end if
  end if
end subroutine


subroutine sort_selection_buffer(b)
  use selection_types
  implicit none

  type(selection_buffer), intent(inout) :: b
  double precision, allocatable :: vals(:), absval(:)
  integer, allocatable :: iorder(:)
  integer(bit_kind), allocatable :: detmp(:,:,:)
  integer :: i, nmwen
  logical, external :: detEq
  nmwen = min(b%N, b%cur)


  allocate(iorder(b%cur), detmp(N_int, 2, nmwen), absval(b%cur), vals(nmwen))
  absval = -dabs(b%val(:b%cur))
  do i=1,b%cur
    iorder(i) = i
  end do
  call dsort(absval, iorder, b%cur)

  do i=1, nmwen
    detmp(:,:,i) = b%det(:,:,iorder(i))
    vals(i) = b%val(iorder(i))
  end do
  b%det(:,:,:nmwen) = detmp(:,:,:)
  b%det(:,:,nmwen+1:) = 0_bit_kind
  b%val(:nmwen) = vals(:)
  b%val(nmwen+1:) = 0d0
  b%mini = max(b%mini,dabs(b%val(b%N)))
  b%cur = nmwen
end subroutine

