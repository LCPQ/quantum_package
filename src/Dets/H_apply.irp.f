use bitmasks

BEGIN_PROVIDER [ double precision, H_apply_threshold ]
  implicit none
  BEGIN_DOC
  ! Theshold on | <Di|H|Dj> |
  END_DOC
  logical :: has
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

BEGIN_PROVIDER [ integer*8, H_apply_buffer_size ]
 implicit none
 BEGIN_DOC
 ! Size of the H_apply buffer.
 END_DOC
 H_apply_buffer_size = 1000

END_PROVIDER

subroutine resize_H_apply_buffer_det(new_size)
  implicit none
  integer, intent(in)            :: new_size
  integer(bit_kind), allocatable :: buffer_det(:,:,:)
  double precision, allocatable  :: buffer_coef(:,:)
  double precision, allocatable  :: buffer_e2(:,:)
  integer                        :: i,j,k
  integer                        :: Ndet
  
  ASSERT (new_size > 0)
  allocate ( buffer_det(N_int,2,new_size), buffer_coef(new_size,N_states), buffer_e2(new_size,N_states) )
  
  do i=1,min(new_size,H_apply_buffer_N_det)
    do k=1,N_int
      buffer_det(k,1,i) = H_apply_buffer_det(k,1,i)
      buffer_det(k,2,i) = H_apply_buffer_det(k,2,i)
    enddo
    ASSERT (sum(popcnt(H_apply_buffer_det(:,1,i))) == elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer_det(:,2,i))) == elec_beta_num )
  enddo
  do k=1,N_states
      do i=1,min(new_size,H_apply_buffer_N_det)
        buffer_coef(i,k) = H_apply_buffer_coef(i,k)
        buffer_e2(i,k)   = H_apply_buffer_e2(i,k)
      enddo
  enddo
  
  H_apply_buffer_size = new_size
  Ndet = min(new_size,H_apply_buffer_N_det)
  TOUCH H_apply_buffer_size
  
  H_apply_buffer_N_det = Ndet

  do i=1,H_apply_buffer_N_det
    do k=1,N_int
      H_apply_buffer_det(k,1,i) = buffer_det(k,1,i)
      H_apply_buffer_det(k,2,i) = buffer_det(k,2,i)
    enddo
    ASSERT (sum(popcnt(H_apply_buffer_det(:,1,i))) == elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer_det(:,2,i))) == elec_beta_num )
  enddo
  do k=1,N_states
    do i=1,H_apply_buffer_N_det
      H_apply_buffer_coef(i,k) = buffer_coef(i,k)
      H_apply_buffer_e2(i,k) = buffer_e2(i,k)
    enddo
  enddo
  
  deallocate (buffer_det, buffer_coef, buffer_e2)
  SOFT_TOUCH H_apply_buffer_det H_apply_buffer_coef H_apply_buffer_N_det H_apply_buffer_e2

end

 BEGIN_PROVIDER [ integer(bit_kind), H_apply_buffer_det,(N_int,2,H_apply_buffer_size) ]
&BEGIN_PROVIDER [ double precision, H_apply_buffer_coef,(H_apply_buffer_size,N_states) ]
&BEGIN_PROVIDER [ double precision, H_apply_buffer_e2,(H_apply_buffer_size,N_states) ]
&BEGIN_PROVIDER [ integer, H_apply_buffer_N_det ]
  implicit none
  BEGIN_DOC
  ! Buffer of determinants/coefficients/perturbative energy for H_apply.
  ! Uninitialized. Filled by H_apply subroutines.
  END_DOC
  H_apply_buffer_N_det = 0

END_PROVIDER


subroutine copy_H_apply_buffer_to_wf
  implicit none
  integer(bit_kind), allocatable :: buffer_det(:,:,:)
  double precision, allocatable  :: buffer_coef(:,:)
  integer                        :: i,j,k
  integer                        :: N_det_old

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
  N_det = N_det + H_apply_buffer_N_det
  TOUCH N_det
  
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
  do i=1,H_apply_buffer_N_det
    do k=1,N_int
      psi_det(k,1,i+N_det_old) = H_apply_buffer_det(k,1,i)
      psi_det(k,2,i+N_det_old) = H_apply_buffer_det(k,2,i)
    enddo
    ASSERT (sum(popcnt(psi_det(:,1,i+N_det_old))) == elec_alpha_num)
    ASSERT (sum(popcnt(psi_det(:,2,i+N_det_old))) == elec_beta_num )
  enddo
  do k=1,N_states
    do i=1,N_det_old
      psi_coef(i,k) = buffer_coef(i,k)
    enddo
    do i=1,H_apply_buffer_N_det
      psi_coef(i+N_det_old,k) = H_apply_buffer_coef(i,k)
    enddo
  enddo
  H_apply_buffer_N_det = 0
  
  SOFT_TOUCH psi_det psi_coef H_apply_buffer_N_det H_apply_buffer_det H_apply_buffer_coef H_apply_buffer_e2

end







