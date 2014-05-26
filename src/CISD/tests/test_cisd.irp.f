program cisd
  implicit none
  integer :: i
  integer :: nmax
  double precision, allocatable :: eigvalues(:),eigvectors(:,:)

  N_states=3
  TOUCH N_states
  call H_apply_cisd
  psi_coef = - 1.d-6

  allocate(eigvalues(N_det),eigvectors(N_det,N_states))

  print *,  'N_det    : ', N_det
! nmax = min(n_det,1000)
! call lapack_diag(eigvalues,eigvectors,H_matrix_all_dets,n_det,nmax)
! do i=1,nmax
!   psi_coef(i,1) = eigvectors(i,1)
! enddo
  do i=1,N_states
    psi_coef(i,i) = 1.d0
  enddo
  call davidson_diag(psi_det,psi_coef,eigvalues,size(psi_coef,1),N_det,N_states,N_int,output_CISD)
  print *,  'HF       :', HF_energy
  print *,  'CISD(1)  : ',eigvalues(1) + nuclear_repulsion
  print *,  'CISD(2)  : ',eigvalues(2) + nuclear_repulsion
  print *,  'CISD(3)  : ',eigvalues(3) + nuclear_repulsion
  deallocate(eigvalues,eigvectors)
end

