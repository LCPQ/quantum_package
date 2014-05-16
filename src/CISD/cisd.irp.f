program cisd
  implicit none
  integer :: i,k
  double precision, allocatable  :: eigvalues(:),eigvectors(:,:)
  PROVIDE ref_bitmask_energy
  call H_apply_cisd
  allocate(eigvalues(n_states),eigvectors(n_det,n_states))
  print *,  'N_det = ', N_det
  print *,  'N_states = ', N_states
  psi_coef = - 1.d-4
  do k=1,N_states
    psi_coef(k,k) = 1.d0
  enddo
  call davidson_diag(psi_det,psi_coef,eigvalues,size(psi_coef,1),N_det,N_states,N_int)

  print *,  '---'
  print *,  'HF:', HF_energy
  print *,  '---'
  do i = 1,1
   print *,  'energy(i)    = ',eigvalues(i) + nuclear_repulsion
  enddo
  deallocate(eigvalues,eigvectors)
end
