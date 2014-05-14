program cisd
  implicit none
  integer :: i
  double precision, allocatable  :: eigvalues(:),eigvectors(:,:)
  call H_apply_cisd
  allocate(eigvalues(n_det),eigvectors(n_det,n_det))
  print *,  'N_det = ', N_det
  psi_coef = psi_coef - 1.d-4
  call davidson_diag(psi_det,psi_coef,eigvalues,size(psi_coef,1),N_det,N_states,N_int)

  print *,  '---'
  print *,  'HF:', HF_energy
  print *,  '---'
  do i = 1,1
   print *,  'energy(i)    = ',eigvalues(i) + nuclear_repulsion
  enddo
  deallocate(eigvalues,eigvectors)
end
