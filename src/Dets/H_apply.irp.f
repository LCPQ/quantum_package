use bitmasks

type H_apply_buffer_type
integer                        :: N_det
integer                        :: sze
integer(bit_kind), pointer     :: det(:,:,:)
double precision , pointer     :: coef(:,:)
double precision , pointer     :: e2(:,:)
end type H_apply_buffer_type

type(H_apply_buffer_type), pointer :: H_apply_buffer(:)


BEGIN_PROVIDER [ logical, H_apply_buffer_allocated ]
  use omp_lib
  implicit none
  BEGIN_DOC
  ! Buffer of determinants/coefficients/perturbative energy for H_apply.
  ! Uninitialized. Filled by H_apply subroutines.
  END_DOC
  integer                        :: iproc, sze
  sze = 100
  if (.not.associated(H_apply_buffer)) then
    allocate(H_apply_buffer(0:nproc-1))
    iproc = 0
    !$OMP PARALLEL PRIVATE(iproc) DEFAULT(SHARED)
    !$   iproc = omp_get_thread_num()
    H_apply_buffer(iproc)%N_det = 0
    H_apply_buffer(iproc)%sze = sze
    allocate (                                                       &
        H_apply_buffer(iproc)%det(N_int,2,sze),                      &
        H_apply_buffer(iproc)%coef(sze,N_states),                    &
        H_apply_buffer(iproc)%e2(sze,N_states)                       &
        )
    H_apply_buffer(iproc)%det  = 0_bit_kind
    H_apply_buffer(iproc)%coef = 0.d0
    H_apply_buffer(iproc)%e2   = 0.d0
    !$OMP END PARALLEL
  endif
  
END_PROVIDER


BEGIN_PROVIDER [ double precision, H_apply_threshold ]
  implicit none
  BEGIN_DOC
  ! Theshold on | <Di|H|Dj> |
  END_DOC
  logical                        :: has
  PROVIDE ezfio_filename
  call ezfio_has_determinants_H_apply_threshold(has)
  if (has) then
    call ezfio_get_determinants_H_apply_threshold(H_apply_threshold)
  else
    H_apply_threshold = 1.d-10
    call ezfio_set_determinants_H_apply_threshold(H_apply_threshold)
  endif
  call write_time(output_Dets)
  call write_double(output_Dets, H_apply_threshold,                  &
      'H_apply_threshold')
END_PROVIDER

subroutine resize_H_apply_buffer(new_size,iproc)
  implicit none
  integer, intent(in)            :: new_size, iproc
  integer(bit_kind), pointer     :: buffer_det(:,:,:)
  double precision,  pointer     :: buffer_coef(:,:)
  double precision,  pointer     :: buffer_e2(:,:)
  integer                        :: i,j,k
  integer                        :: Ndet
  PROVIDE H_apply_buffer_allocated
  
  ASSERT (new_size > 0)
  ASSERT (iproc >= 0)
  ASSERT (iproc < nproc)
  
  allocate ( buffer_det(N_int,2,new_size),                           &
      buffer_coef(new_size,N_states),                                &
      buffer_e2(new_size,N_states) )
  
  do i=1,min(new_size,H_apply_buffer(iproc)%N_det)
    do k=1,N_int
      buffer_det(k,1,i) = H_apply_buffer(iproc)%det(k,1,i)
      buffer_det(k,2,i) = H_apply_buffer(iproc)%det(k,2,i)
    enddo
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i))) == elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num )
  enddo
  deallocate(H_apply_buffer(iproc)%det)
  H_apply_buffer(iproc)%det => buffer_det
  
  do k=1,N_states
    do i=1,min(new_size,H_apply_buffer(iproc)%N_det)
      buffer_coef(i,k) = H_apply_buffer(iproc)%coef(i,k)
    enddo
  enddo
  deallocate(H_apply_buffer(iproc)%coef)
  H_apply_buffer(iproc)%coef => buffer_coef
  
  do k=1,N_states
    do i=1,min(new_size,H_apply_buffer(iproc)%N_det)
      buffer_e2(i,k)   = H_apply_buffer(iproc)%e2(i,k)
    enddo
  enddo
  deallocate(H_apply_buffer(iproc)%e2)
  H_apply_buffer(iproc)%e2 => buffer_e2
  
  H_apply_buffer(iproc)%sze = new_size
  H_apply_buffer(iproc)%N_det = min(new_size,H_apply_buffer(iproc)%N_det)
  
end

subroutine copy_H_apply_buffer_to_wf
  use omp_lib
  implicit none
  BEGIN_DOC
! Copies the H_apply buffer to psi_coef. You need to touch psi_det, psi_coef and N_det
! after calling this function.
  END_DOC
  integer(bit_kind), allocatable :: buffer_det(:,:,:)
  double precision, allocatable  :: buffer_coef(:,:)
  integer                        :: i,j,k
  integer                        :: N_det_old
  integer                        :: iproc
  
  PROVIDE H_apply_buffer_allocated
  
  ASSERT (N_int > 0)
  ASSERT (N_det > 0)
  
  allocate ( buffer_det(N_int,2,N_det), buffer_coef(N_det,N_states) )
  
  do i=1,N_det
    do k=1,N_int
      ASSERT (sum(popcnt(psi_det(:,1,i))) == elec_alpha_num)
      ASSERT (sum(popcnt(psi_det(:,2,i))) == elec_beta_num)
      buffer_det(k,1,i) = psi_det(k,1,i)
      buffer_det(k,2,i) = psi_det(k,2,i)
    enddo
  enddo
  do k=1,N_states
    do i=1,N_det
      buffer_coef(i,k) = psi_coef(i,k)
    enddo
  enddo
  
  N_det_old = N_det
  do j=0,nproc-1
    N_det = N_det + H_apply_buffer(j)%N_det
  enddo
  
  if (psi_det_size < N_det) then
    psi_det_size = N_det
    TOUCH psi_det_size
  endif
  do i=1,N_det_old
    do k=1,N_int
      psi_det(k,1,i) = buffer_det(k,1,i)
      psi_det(k,2,i) = buffer_det(k,2,i)
    enddo
    ASSERT (sum(popcnt(psi_det(:,1,i))) == elec_alpha_num)
    ASSERT (sum(popcnt(psi_det(:,2,i))) == elec_beta_num )
  enddo
  do k=1,N_states
    do i=1,N_det_old
      psi_coef(i,k) = buffer_coef(i,k)
    enddo
  enddo
  !$OMP PARALLEL DEFAULT(SHARED)                                     &
      !$OMP PRIVATE(j,k,i) FIRSTPRIVATE(N_det_old)                   &
      !$OMP SHARED(N_int,H_apply_buffer,psi_det,psi_coef,N_states)
  j=0
  !$ j=omp_get_thread_num()
  do k=0,j-1
    N_det_old += H_apply_buffer(k)%N_det
  enddo
  do i=1,H_apply_buffer(j)%N_det
    do k=1,N_int
      psi_det(k,1,i+N_det_old) = H_apply_buffer(j)%det(k,1,i)
      psi_det(k,2,i+N_det_old) = H_apply_buffer(j)%det(k,2,i)
    enddo
    ASSERT (sum(popcnt(psi_det(:,1,i+N_det_old))) == elec_alpha_num)
    ASSERT (sum(popcnt(psi_det(:,2,i+N_det_old))) == elec_beta_num )
  enddo
  do k=1,N_states
    do i=1,H_apply_buffer(j)%N_det
      psi_coef(i+N_det_old,k) = H_apply_buffer(j)%coef(i,k)
    enddo
  enddo
  !$OMP BARRIER
  H_apply_buffer(j)%N_det = 0
  !$OMP END PARALLEL
  call normalize(psi_coef,N_det)
  SOFT_TOUCH psi_det psi_coef N_det
  
end


subroutine fill_H_apply_buffer_no_selection(n_selected,det_buffer,Nint,iproc)
  use bitmasks
  implicit none
  BEGIN_DOC
  !  Fill the H_apply buffer with determiants for CISD
  END_DOC
  
  integer, intent(in)            :: n_selected, Nint, iproc
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k
  integer                        :: new_size
  PROVIDE H_apply_buffer_allocated
  new_size = H_apply_buffer(iproc)%N_det + n_selected
  if (new_size > H_apply_buffer(iproc)%sze) then
    call resize_h_apply_buffer(max(2*H_apply_buffer(iproc)%sze,new_size),iproc)
  endif
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
  do i=1,n_selected
    do j=1,N_int
      H_apply_buffer(iproc)%det(j,1,i+H_apply_buffer(iproc)%N_det) = det_buffer(j,1,i)
      H_apply_buffer(iproc)%det(j,2,i+H_apply_buffer(iproc)%N_det) = det_buffer(j,2,i)
    enddo
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i+H_apply_buffer(iproc)%N_det)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i+H_apply_buffer(iproc)%N_det))) == elec_beta_num)
  enddo
  do j=1,N_states
    do i=1,N_selected
      H_apply_buffer(iproc)%coef(i,j) = 0.d0
    enddo
  enddo
  H_apply_buffer(iproc)%N_det = new_size
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
end


