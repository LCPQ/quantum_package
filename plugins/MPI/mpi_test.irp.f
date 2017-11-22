program MPI
  implicit none
  BEGIN_DOC
! MPI test program
  END_DOC
  print *,  'hello world'
  print *,  'rank, size, master = ', mpi_rank, mpi_size, mpi_master

  integer :: ierr
  call MPI_FINALIZE(ierr)
  print *,  ierr
end
