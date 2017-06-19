 BEGIN_PROVIDER [ logical, MPI_Initialized ]
&BEGIN_PROVIDER [ logical, has_mpi ]
  implicit none
  BEGIN_DOC
! This is true when MPI_Init has been called
  END_DOC

  IRP_IF MPI
      integer :: ierr
      call MPI_Init(ierr)
      if (ierr /= 0) then
         print *, ierr
         print *, 'MPI failed to initialize'
         stop -1
      endif
  IRP_ENDIF
  MPI_Initialized = .True.
END_PROVIDER


 BEGIN_PROVIDER [ integer, MPI_rank ]
&BEGIN_PROVIDER [ integer, MPI_size ]
&BEGIN_PROVIDER [ logical, is_MPI_master ]
 implicit none
 BEGIN_DOC
! Usual MPI variables
 END_DOC

 PROVIDE MPI_Initialized

 IRP_IF MPI
   integer :: ierr
   call mpi_comm_size(MPI_COMM_WORLD, MPI_size, ierr)
   if (ierr /= 0) then
     print *, ierr
     print *, 'Unable to get MPI_size'
     stop -1
   endif
   call mpi_comm_rank(MPI_COMM_WORLD, MPI_rank, ierr)
   if (ierr /= 0) then
     print *, ierr
     print *, 'Unable to get MPI_rank'
     stop -1
   endif
   is_MPI_master = (MPI_rank == 0)
 IRP_ELSE
   MPI_rank = 0
   MPI_size = 1
   is_MPI_master = .True.
 IRP_ENDIF


END_PROVIDER

subroutine qp_mpi_finalize()
  implicit none
  PROVIDE MPI_Initialized
  IRP_IF MPI
      integer :: ierr
      call MPI_Finalize(ierr)
      if (ierr /= 0) then
         print *, ierr
         print *, 'Unable to finalize MPI'
         stop -1
       endif
  IRP_ENDIF
end subroutine

