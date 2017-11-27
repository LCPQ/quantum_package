BEGIN_PROVIDER [ integer, mpi_bit_kind ]
 use bitmasks
 implicit none
 BEGIN_DOC
 ! MPI bit kind type
 END_DOC
 IRP_IF MPI
  include 'mpif.h'
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

  call MPI_COMM_SIZE (MPI_COMM_WORLD, mpi_size, ierr)
  if (ierr /= MPI_SUCCESS) then
    print *,  'ierr = ', ierr
    stop 'Unable to get MPI size'
  endif

 IRP_ELSE
  mpi_rank = 0
  mpi_size = 1
 IRP_ENDIF
 if (mpi_size > 1) then
    call write_int(6,mpi_rank,'MPI rank')
    call write_int(6,mpi_size,'MPI size')
 endif
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

BEGIN_TEMPLATE

subroutine broadcast_chunks_$double(A, LDA)
  implicit none
  integer, intent(in)             :: LDA
  $type, intent(inout) :: A(LDA)
  use bitmasks
  BEGIN_DOC
! Broadcast with chunks of ~2GB
  END_DOC
  IRP_IF MPI
    include 'mpif.h'
    integer :: i, sze, ierr
    do i=1,LDA,200000000/$8
      sze = min(LDA-i+1, 200000000/$8)
      call MPI_BCAST (A(i), sze, MPI_$DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      if (ierr /= MPI_SUCCESS) then
        print *,  irp_here//': Unable to broadcast chuks $double ', i
        stop -1
      endif
    enddo
  IRP_ENDIF
end

SUBST [ double, type, 8, DOUBLE_PRECISION ]
double    ; double precision  ; 8             ; DOUBLE_PRECISION ;;
bit_kind  ; integer(bit_kind) ; bit_kind_size ; BIT_KIND ;;
integer   ; integer          ; 4             ; INTEGER4 ;;
integer8  ; integer*8        ; 8             ; INTEGER8 ;;

END_TEMPLATE


