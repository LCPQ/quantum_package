BEGIN_PROVIDER [ double precision, CI_SC2_energy, (N_states) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_Dets)
  do j=1,N_states
    CI_SC2_energy(j) = CI_SC2_electronic_energy(j) + nuclear_repulsion
    write(st,'(I4)') j
    call write_double(output_Dets,CI_SC2_energy(j),'Energy of state '//trim(st))
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, threshold_convergence_SC2]
 implicit none
 BEGIN_DOC
 ! convergence of the correlation energy of SC2 iterations
 END_DOC
 threshold_convergence_SC2 = 1.d-8

 END_PROVIDER
 BEGIN_PROVIDER [ double precision, CI_SC2_electronic_energy, (N_states) ]
&BEGIN_PROVIDER [ double precision, CI_SC2_eigenvectors, (N_det,N_states) ]
  implicit none
  BEGIN_DOC
  ! Eigenvectors/values of the CI matrix
  END_DOC
  integer                        :: i,j
  
  do j=1,N_states
    do i=1,N_det
!     CI_SC2_eigenvectors(i,j) = psi_coef(i,j)
      CI_SC2_eigenvectors(i,j) = CI_eigenvectors(i,j)
    enddo
    CI_SC2_electronic_energy(j) = CI_electronic_energy(j) 
  enddo
  
  double precision :: convergence
  call CISD_SC2(psi_det,CI_SC2_eigenvectors,CI_SC2_electronic_energy, &
        size(CI_SC2_eigenvectors,1),N_det,N_states,N_int,threshold_convergence_SC2)
END_PROVIDER

subroutine diagonalize_CI_SC2
  implicit none
  BEGIN_DOC
!  Replace the coefficients of the CI states by the coefficients of the 
!  eigenstates of the CI matrix
  END_DOC
  integer :: i,j
  do j=1,N_states
    do i=1,N_det
      psi_coef(i,j) = CI_SC2_eigenvectors(i,j)
    enddo
  enddo
  SOFT_TOUCH psi_coef CI_SC2_electronic_energy CI_SC2_energy CI_SC2_eigenvectors
end
