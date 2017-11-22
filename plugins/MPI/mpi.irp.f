BEGIN_PROVIDER [ logical, mpi_initialized ]
 implicit none
 BEGIN_DOC
 ! Always true. Initialized MPI
 END_DOC
 IRP_IF MPI
  include 'mpif.h'
  integer                        :: ierr
  call mpi_init(ierr)
  if (ierr /= 0) then
    print *,  'ierr = ', ierr
    stop 'Unable to initialize MPI'
  endif
 IRP_ENDIF
 mpi_initialized = .True.
END_PROVIDER


 BEGIN_PROVIDER [ integer, mpi_rank ]
&BEGIN_PROVIDER [ integer, mpi_size ]
 implicit none
 BEGIN_DOC
 ! Rank of MPI process and number of MPI processes
 END_DOC
 IRP_IF MPI
  include 'mpif.h'
  PROVIDE mpi_initialized
  integer                        :: ierr

  call MPI_COMM_RANK (MPI_COMM_WORLD, mpi_rank, ierr)
  if (ierr /= 0) then
    print *,  'ierr = ', ierr
    stop 'Unable to get MPI rank'
  endif

  call MPI_COMM_SIZE (MPI_COMM_WORLD, mpi_size, ierr)
  if (ierr /= 0) then
    print *,  'ierr = ', ierr
    stop 'Unable to get MPI size'
  endif

 IRP_ELSE
  mpi_rank = 0
  mpi_size = 1
 IRP_ENDIF
 ASSERT (mpi_rank >= 0)
 ASSERT (mpi_rank < mpi_size)

END_PROVIDER


BEGIN_PROVIDER [ logical, mpi_master ]
 implicit none
 BEGIN_DOC
 ! If true, rank is zero
 END_DOC
 mpi_master = (mpi_rank == 0)

END_PROVIDER

