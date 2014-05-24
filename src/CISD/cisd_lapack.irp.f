program cisd
  implicit none
  integer :: i
  call H_apply_cisd
  double precision, allocatable :: eigvalues(:),eigvectors(:,:)
  allocate(eigvalues(n_det),eigvectors(n_det,n_det))
  print *,  'N_det = ', N_det
  call lapack_diag(eigvalues,eigvectors,H_matrix_all_dets,n_det,n_det)

! print *,  H_matrix_all_dets
  print *,  '---'
  print *,  'HF:', HF_energy
  print *,  '---'
  do i = 1,20
   print *,  'energy(i)    = ',eigvalues(i) + nuclear_repulsion
  enddo
! print *,  eigvectors(:,1)
  deallocate(eigvalues,eigvectors)
end

