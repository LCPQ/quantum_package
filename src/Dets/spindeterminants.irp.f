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
  
  integer                        :: n_svd_coefs
  double precision               :: norm, f
  f = 1.d0/dble(N_states)
  norm = 1.d0
  do n_svd_coefs=1,N_det_alpha_unique
    do k=1,N_states
      norm -= psi_svd_coefs(n_svd_coefs,k)*psi_svd_coefs(n_svd_coefs,k)
    enddo
    if (norm < 1.d-6) then
      exit
    endif
  enddo
  n_svd_coefs -= 1
  call ezfio_set_spindeterminants_n_svd_coefs(n_svd_coefs)
  
  double precision, allocatable  :: dtmp(:,:,:)
  allocate(dtmp(N_det_alpha_unique,n_svd_coefs,N_states))
  do k=1,N_states
    do j=1,n_svd_coefs
      do i=1,N_det_alpha_unique
        dtmp(i,j,k) = psi_svd_alpha(i,j,k)
      enddo
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_svd_alpha(dtmp)
  deallocate(dtmp)

  allocate(dtmp(N_det_beta_unique,n_svd_coefs,N_states))
  do k=1,N_states
    do j=1,n_svd_coefs
      do i=1,N_det_beta_unique
        dtmp(i,j,k) = psi_svd_beta(i,j,k)
      enddo
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_svd_beta(dtmp)
  deallocate(dtmp)

  allocate(dtmp(n_svd_coefs,N_states,1))
  do k=1,N_states
    do j=1,n_svd_coefs
        dtmp(j,k,1) = psi_svd_coefs(j,k)
    enddo
  enddo
  call ezfio_set_spindeterminants_psi_svd_coefs(dtmp)
  deallocate(dtmp)

end
