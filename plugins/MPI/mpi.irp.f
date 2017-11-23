BEGIN_PROVIDER [ integer, mpi_bit_kind ]
 use bitmasks
 include 'mpif.h'
 implicit none
 BEGIN_DOC
 ! MPI bit kind type
 END_DOC
 IRP_IF MPI
 if (bit_kind == 4) then
  mpi_bit_kind = MPI_INTEGER4
 else if (bit_kind == 8) then
  mpi_bit_kind = MPI_INTEGER8
 else
  stop 'Wrong bit kind in mpi_bit_kind'
 endif
 IRP_ELSE
  mpi_bit_kind = -1
 IRP_ENDIF
END_PROVIDER

BEGIN_PROVIDER [ logical, mpi_initialized ]
 implicit none
 BEGIN_DOC
 ! Always true. Initialized MPI
 END_DOC
 IRP_IF MPI
  include 'mpif.h'
  integer                        :: ierr
  call mpi_init(ierr)
  if (ierr /= MPI_SUCCESS) then
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
  if (ierr /= MPI_SUCCESS) then
    print *,  'ierr = ', ierr
    stop 'Unable to get MPI rank'
  endif
  call write_int(6,mpi_rank,'MPI rank')

  call MPI_COMM_SIZE (MPI_COMM_WORLD, mpi_size, ierr)
  if (ierr /= MPI_SUCCESS) then
    print *,  'ierr = ', ierr
    stop 'Unable to get MPI size'
  endif
  call write_int(6,mpi_size,'MPI size')

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

