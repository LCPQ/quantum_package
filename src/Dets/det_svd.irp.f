program det_svd
  implicit none
  BEGIN_DOC
! Computes the SVD of the Alpha x Beta determinant coefficient matrix
  END_DOC
  integer                        :: i,j

  read_wf = .True.
  TOUCH read_wf

  print *,  'SVD matrix before filling'
  print *,  '========================='
  print *,  ''
  print *,  'N_det = ', N_det
  print *,  'N_det_alpha = ', N_det_alpha_unique
  print *,  'N_det_beta  = ', N_det_beta_unique
  print *,  ''

!  do i=1,N_det_alpha_unique
!    do j=1,N_det_beta_unique
!      print *,  i,j,psi_svd_matrix(i,j,:)
!    enddo
!  enddo

  print *,  ''
  print *,  'Energy = ', ci_energy
  print *,  ''

  print *,  psi_svd_coefs(1:20,1)

  call generate_all_alpha_beta_det_products
  print *,  ''
  print *,  'Energy = ', ci_energy
  print *,  ''

  print *,  'SVD matrix after filling'
  print *,  '========================'
  print *,  ''
  print *,  'N_det = ', N_det
  print *,  'N_det_alpha = ', N_det_alpha_unique
  print *,  'N_det_beta  = ', N_det_beta_unique
  print *,  ''

!  do i=1,N_det_alpha_unique
!    do j=1,N_det_beta_unique
!      print *,  i,j,psi_svd_matrix(i,j,:)
!    enddo
!  enddo

  print *,  ''
  print *,  'Energy = ', ci_energy
  print *,  ''
  print *,  psi_svd_coefs(1:20,1)
!  call save_wavefunction

end
