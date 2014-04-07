program test_number_electrons
  implicit none
  integer :: i
  i = len(ezfio_filename)
  do while (ezfio_filename(i:i) == ' ')
    i -= 1
    if (i == 0) then
      stop 1
    endif
  enddo
  i -= 1
  do while (ezfio_filename(i:i) /= '/')
    i -= 1
    if (i == 0) then
      stop 1
    endif
  enddo
  i += 1
  print *, '"'//trim(ezfio_filename(i:))//'"'

end
