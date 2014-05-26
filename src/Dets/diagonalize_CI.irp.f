BEGIN_PROVIDER [ character*(64), diag_algorithm ]
  implicit none
  BEGIN_DOC
  ! Diagonalization algorithm (Davidson or Lapack)
  END_DOC
  if (N_det > 500) then
    diag_algorithm = "Davidson"
  else
    diag_algorithm = "Lapack"
  endif

  if (N_det < N_states) then
    diag_algorithm = "Lapack"
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, CI_energy, (N_states) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  do j=1,N_states
    CI_energy(j) = CI_electronic_energy(j) + nuclear_repulsion
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, CI_electronic_energy, (N_states) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors, (N_det,N_states) ]
  implicit none
  BEGIN_DOC
  ! Eigenvectors/values of the CI matrix
  END_DOC
  integer                        :: i,j
  
  do j=1,N_states
    do i=1,N_det
      CI_eigenvectors(i,j) = psi_coef(i,j)
    enddo
  enddo
  
  if (diag_algorithm == "Davidson") then
    
    call davidson_diag(psi_det,CI_eigenvectors,CI_electronic_energy, &
        size(CI_eigenvectors,1),N_det,N_states,N_int,output_Dets)
    
  else if (diag_algorithm == "Lapack") then
    
    double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
    allocate (eigenvectors(size(H_matrix_all_dets,1),N_det))
    allocate (eigenvalues(N_det))
    call lapack_diag(eigenvalues,eigenvectors,                       &
        H_matrix_all_dets,size(H_matrix_all_dets,1),N_det)
    CI_electronic_energy(:) = 0.d0
    do j=1,min(N_states,N_det)
      do i=1,N_det
        CI_eigenvectors(i,j) = eigenvectors(i,j)
      enddo
      CI_electronic_energy(j) = eigenvalues(j)
    enddo
    deallocate(eigenvectors,eigenvalues)
  endif
  
END_PROVIDER

subroutine diagonalize_CI
  implicit none
  BEGIN_DOC
!  Replace the coefficients of the CI states by the coefficients of the 
!  eigenstates of the CI matrix
  END_DOC
  integer :: i,j
  do j=1,N_states
    do i=1,N_det
      psi_coef(i,j) = CI_eigenvectors(i,j)
    enddo
  enddo
  SOFT_TOUCH psi_coef psi_det CI_electronic_energy CI_energy CI_eigenvectors
end
