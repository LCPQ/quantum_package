module selection_types
  type selection_buffer
    integer :: N, cur
    integer(8), allocatable :: det(:,:,:)
    double precision, allocatable :: val(:)
    double precision :: mini
  endtype
end module

