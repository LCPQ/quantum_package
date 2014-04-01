BEGIN_PROVIDER [ character*(128), ezfio_filename ]
  implicit none
  BEGIN_DOC
! Name of EZFIO file
  END_DOC
  integer :: iargc
  call getarg(0,ezfio_filename)
  if (iargc() /= 1) then
    print *, ezfio_filename, ' <ezfio_file>'
    stop 1
  endif
  call getarg(1,ezfio_filename)
  call ezfio_set_file(ezfio_filename)
END_PROVIDER
 
