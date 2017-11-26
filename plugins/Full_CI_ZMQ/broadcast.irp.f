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

    call MPI_BCAST (N_states, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_states'
      stop -1
    endif

    call MPI_BCAST (N_det, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_det'
      stop -1
    endif

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

    call MPI_BCAST (N_det_generators, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_det_generators'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH N_det_generators
    endif

    call MPI_BCAST (N_det_selectors, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast N_det_selectors'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH N_det_selectors
    endif

    call MPI_BCAST (energy, size(energy), MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= MPI_SUCCESS) then
      print *,  irp_here//': Unable to broadcast energy'
      stop -1
    endif

  IRP_ENDIF
end
