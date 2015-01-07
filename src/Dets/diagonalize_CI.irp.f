BEGIN_PROVIDER [ character*(64), diag_algorithm ]
  implicit none
  BEGIN_DOC
  ! Diagonalization algorithm (Davidson or Lapack)
  END_DOC
  if (N_det > N_det_max_jacobi) then
    diag_algorithm = "Davidson"
  else
    diag_algorithm = "Lapack"
  endif

  if (N_det < N_states_diag) then
    diag_algorithm = "Lapack"
  endif
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, CI_energy, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_Dets)
  do j=1,N_states_diag
    CI_energy(j) = CI_electronic_energy(j) + nuclear_repulsion
    write(st,'(I4)') j
    call write_double(output_Dets,CI_energy(j),'Energy of state '//trim(st))
    call write_double(output_Dets,CI_eigenvectors_s2(j),'S^2 of state '//trim(st))
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, CI_electronic_energy, (N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors, (N_det,N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_s2, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! Eigenvectors/values of the CI matrix
  END_DOC
  integer                        :: i,j
  
  do j=1,N_states_diag
    do i=1,N_det
      CI_eigenvectors(i,j) = psi_coef(i,j)
    enddo
  enddo
  
  if (diag_algorithm == "Davidson") then
    
    call davidson_diag(psi_det,CI_eigenvectors,CI_electronic_energy, &
        size(CI_eigenvectors,1),N_det,N_states_diag,N_int,output_Dets)
    
  else if (diag_algorithm == "Lapack") then
    
    double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
    allocate (eigenvectors(size(H_matrix_all_dets,1),N_det))
    allocate (eigenvalues(N_det))
    call lapack_diag(eigenvalues,eigenvectors,                       &
        H_matrix_all_dets,size(H_matrix_all_dets,1),N_det)
    CI_electronic_energy(:) = 0.d0
    do i=1,N_det
       CI_eigenvectors(i,1) = eigenvectors(i,1)
    enddo
    integer :: i_state
    double precision :: s2
    i_state = 0
    do j=1,N_det
      call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
      print *, 'j = ',j,s2, expected_s2
      if(dabs(s2-expected_s2).le.0.3d0)then
       i_state += 1
       print *,  'i_state = ',i_state
       do i=1,N_det
         CI_eigenvectors(i,i_state) = eigenvectors(i,j)
       enddo
       CI_electronic_energy(i_state) = eigenvalues(j)
       CI_eigenvectors_s2(i_state) = s2
      endif
      if (i_state.ge.N_states_diag) then
        exit
      endif
    enddo
!    if(i_state < min(N_states_diag,N_det))then
!     print *, 'pb with the number of states'
!     print *, 'i_state = ',i_state
!     print *, 'N_states_diag ',N_states_diag
!     print *,'stopping ...'
!     stop
!    endif
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
  do j=1,N_states_diag
    do i=1,N_det
      psi_coef(i,j) = CI_eigenvectors(i,j)
    enddo
  enddo
  SOFT_TOUCH psi_coef CI_electronic_energy CI_energy CI_eigenvectors CI_eigenvectors_s2
end
