subroutine mpi_bcast_psi(energy, size_energy)
  implicit none
  BEGIN_DOC
! Broadcast the wave function via MPI
  END_DOC
  integer, intent(in)            :: size_energy 
  double precision, intent(inout)  :: energy(size_energy) 
  integer :: sze
  PROVIDE mpi_initialized

  IRP_IF MPI
    include 'mpif.h'
    integer :: ierr

    call MPI_BCAST (N_states, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast N_states'
      stop -1
    endif

    call MPI_BCAST (N_det, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast N_det'
      stop -1
    endif

    call MPI_BCAST (psi_det_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast psi_det_size'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH psi_det_size N_det N_states
    endif

    print *,  size(psi_det), psi_det_size
    call MPI_BCAST (psi_det, size(psi_det), MPI_BIT_KIND, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast psi_det'
      stop -1
    endif

    call MPI_BCAST (psi_coef, size(psi_coef), MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast psi_coef'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH psi_det psi_coef
    endif

    call MPI_BCAST (N_det_generators, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast N_det_generators'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH N_det_generators
    endif

    call MPI_BCAST (N_det_selectors, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast N_det_selectors'
      stop -1
    endif

    if (.not.mpi_master) then
      TOUCH N_det_selectors
    endif

    call MPI_BCAST (energy, size(energy), MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) then
      print *,  'Unable to broadcast energy'
      stop -1
    endif

  IRP_ENDIF
end
