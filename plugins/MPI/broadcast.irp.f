BEGIN_TEMPLATE

subroutine broadcast_chunks_$double(A, LDA)
  implicit none
  integer, intent(in)             :: LDA
  $type, intent(inout) :: A(LDA)
  use bitmasks
  include 'mpif.h'
  BEGIN_DOC
! Broadcast with chunks of ~2GB
  END_DOC
  integer :: i, sze, ierr
  do i=1,LDA,2000000000/$8
    sze = min(LDA-i+1, 2000000000/$8)
!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (A(i), sze, MPI_$DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast chuks $double ', i
      stop -1
    endif
  enddo

end

SUBST [ double, type, 8, DOUBLE_PRECISION ]
double    ; double precision  ; 8             ; DOUBLE_PRECISION ;;
bit_kind  ; integer(bit_kind) ; bit_kind_size ; BIT_KIND ;;
integer   ; integer          ; 4             ; INTEGER4 ;;
integer8  ; integer*8        ; 8             ; INTEGER8 ;;

END_TEMPLATE


subroutine mpi_bcast_psi(energy, size_energy)
  implicit none
  BEGIN_DOC
! Broadcast the wave function via MPI
  END_DOC
  integer, intent(in)            :: size_energy 
  double precision, intent(inout)  :: energy(size_energy) 
  PROVIDE mpi_initialized

  IRP_IF MPI
    include 'mpif.h'
    integer :: ierr

!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (N_states, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_states'
      stop -1
    endif

!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (N_det, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_det'
      stop -1
    endif

!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (psi_det_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast psi_det_size'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH psi_det_size N_det N_states
    endif

    
    call broadcast_chunks_bit_kind(psi_det,N_det*N_int*2)

    call broadcast_chunks_double(psi_coef,N_states*N_det)

    if (.not.mpi_master) then
      TOUCH psi_det psi_coef
    endif

!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (N_det_generators, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_det_generators'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH N_det_generators
    endif

!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (N_det_selectors, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_det_selectors'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH N_det_selectors
    endif

!    call MPI_BARRIER(MPI_COMM_WORLD)
    call MPI_BCAST (energy, size(energy), MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast energy'
      stop -1
    endif
!    call MPI_BARRIER(MPI_COMM_WORLD)

  IRP_ENDIF
end
