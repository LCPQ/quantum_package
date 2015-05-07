 BEGIN_PROVIDER [ double precision, CI_electronic_energy_mono, (N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_mono, (N_det,N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_s2_mono, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! Eigenvectors/values of the CI matrix
  END_DOC
  integer                        :: i,j
  
  do j=1,N_states_diag
    do i=1,N_det
      CI_eigenvectors_mono(i,j) = psi_coef(i,j)
    enddo
  enddo
  
  if (diag_algorithm == "Davidson") then
    
    call davidson_diag(psi_det,CI_eigenvectors_mono,CI_electronic_energy, &
        size(CI_eigenvectors_mono,1),N_det,N_states_diag,N_int,output_determinants)
    
  else if (diag_algorithm == "Lapack") then
    
    double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
    allocate (eigenvectors(size(H_matrix_all_dets,1),N_det))
    allocate (eigenvalues(N_det))
    call lapack_diag(eigenvalues,eigenvectors,                       &
        H_matrix_all_dets,size(H_matrix_all_dets,1),N_det)
    CI_electronic_energy_mono(:) = 0.d0
    do i=1,N_det
       CI_eigenvectors_mono(i,1) = eigenvectors(i,1)
    enddo
    integer :: i_state
    double precision :: s2
    i_state = 0
    if (s2_eig) then
      do j=1,N_det
        call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
        if(dabs(s2-expected_s2).le.0.3d0)then
          print*,'j = ',j
          print*,'e = ',eigenvalues(j)
          print*,'c = ',dabs(eigenvectors(1,j))
          if(dabs(eigenvectors(1,j)).gt.0.9d0)then
            i_state += 1
            do i=1,N_det
              CI_eigenvectors_mono(i,i_state) = eigenvectors(i,j)
            enddo
            CI_electronic_energy_mono(i_state) = eigenvalues(j)
            CI_eigenvectors_s2_mono(i_state) = s2
          endif
        endif
        if (i_state.ge.N_states_diag) then
          exit
        endif
      enddo
    else
      do j=1,N_states_diag
        call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
        if(dabs(eigenvectors(1,j)).gt.0.9d0)then
          i_state += 1
          do i=1,N_det
            CI_eigenvectors_mono(i,i_state) = eigenvectors(i,j)
          enddo
          CI_electronic_energy_mono(i_state) = eigenvalues(j)
          CI_eigenvectors_s2_mono(i_state) = s2
        endif
      enddo
    endif
    deallocate(eigenvectors,eigenvalues)
  endif
  
END_PROVIDER

subroutine diagonalize_CI_mono
  implicit none
  BEGIN_DOC
!  Replace the coefficients of the CI states by the coefficients of the 
!  eigenstates of the CI matrix
  END_DOC
  integer :: i,j
  do j=1,N_states_diag
    do i=1,N_det
      psi_coef(i,j) = CI_eigenvectors_mono(i,j)
    enddo
  enddo
  SOFT_TOUCH psi_coef CI_electronic_energy_mono CI_eigenvectors_mono CI_eigenvectors_s2_mono
end
