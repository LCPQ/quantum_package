subroutine write_spindeterminants
  use bitmasks
  implicit none
  integer*8, allocatable         :: tmpdet(:,:)
  integer                        :: N_int2
  integer                        :: i,j,k
  integer*8                      :: det_8(100)
  integer(bit_kind)              :: det_bk((100*8)/bit_kind)
  equivalence (det_8, det_bk)

  N_int2 = (N_int*bit_kind)/8
  call ezfio_set_spindeterminants_n_det_alpha(N_det_alpha_unique)
  call ezfio_set_spindeterminants_n_det_beta(N_det_beta_unique)
  call ezfio_set_spindeterminants_n_int(N_int)
  call ezfio_set_spindeterminants_bit_kind(bit_kind)
  call ezfio_set_spindeterminants_n_states(N_states)

  allocate(tmpdet(N_int2,N_det_alpha_unique))
  do i=1,N_det_alpha_unique
    do k=1,N_int
      det_bk(k) = psi_det_alpha_unique(k,i)
    enddo
    do k=1,N_int2
      tmpdet(k,i) = det_8(k)
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_det_alpha(psi_det_alpha_unique)
  deallocate(tmpdet)

  allocate(tmpdet(N_int2,N_det_beta_unique))
  do i=1,N_det_beta_unique
    do k=1,N_int
      det_bk(k) = psi_det_beta_unique(k,i)
    enddo
    do k=1,N_int2
      tmpdet(k,i) = det_8(k)
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_det_beta(psi_det_beta_unique)
  deallocate(tmpdet)

  call ezfio_set_spindeterminants_psi_coef_matrix(psi_svd_matrix)

end
