
BEGIN_PROVIDER [ double precision, CI_energy, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_determinants)
  do j=1,min(N_det,N_states_diag)
    CI_energy(j) = CI_electronic_energy(j) + nuclear_repulsion
    write(st,'(I4)') j
    call write_double(output_determinants,CI_energy(j),'Energy of state '//trim(st))
    call write_double(output_determinants,CI_eigenvectors_s2(j),'S^2 of state '//trim(st))
  enddo

END_PROVIDER

 BEGIN_PROVIDER [ double precision, CI_electronic_energy, (N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors, (N_det,N_states_diag) ]
&BEGIN_PROVIDER [ double precision, CI_eigenvectors_s2, (N_states_diag) ]
   BEGIN_DOC
   ! Eigenvectors/values of the CI matrix
   END_DOC
   implicit none
   double precision               :: ovrlp,u_dot_v
   integer                        :: i_good_state
   integer, allocatable           :: index_good_state_array(:)
   logical, allocatable           :: good_state_array(:)
   double precision, allocatable  :: s2_values_tmp(:)
   integer                        :: i_other_state
   double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
   integer                        :: i_state
   double precision               :: e_0
   integer                        :: i,j,k
   double precision, allocatable  :: s2_eigvalues(:)
   double precision, allocatable  :: e_array(:)
   integer, allocatable           :: iorder(:)
   
   ! Guess values for the "N_states_diag" states of the CI_eigenvectors
   do j=1,min(N_states_diag,N_det)
     do i=1,N_det
       CI_eigenvectors(i,j) = psi_coef(i,j)
     enddo
   enddo
   
   do j=N_det+1,N_states_diag
     do i=1,N_det
       CI_eigenvectors(i,j) = 0.d0
     enddo
   enddo
   
   if (diag_algorithm == "Davidson") then
     
     call davidson_diag(psi_det,CI_eigenvectors,CI_electronic_energy,&
         size(CI_eigenvectors,1),N_det,N_states_diag,N_int,output_determinants)

     call u_0_S2_u_0_nstates(CI_eigenvectors_s2,CI_eigenvectors,N_det,psi_det,N_int,&
         N_states_diag,size(CI_eigenvectors,1))
     
     
   else if (diag_algorithm == "Lapack") then
     
     allocate (eigenvectors(size(H_matrix_all_dets,1),N_det))
     allocate (eigenvalues(N_det))
     call lapack_diag(eigenvalues,eigenvectors,                      &
         H_matrix_all_dets,size(H_matrix_all_dets,1),N_det)
     CI_electronic_energy(:) = 0.d0
     if (s2_eig) then
       i_state = 0
       allocate (s2_eigvalues(N_det))
       allocate(index_good_state_array(N_det),good_state_array(N_det))
       good_state_array = .False.
       call u_0_S2_u_0_nstates(s2_eigvalues,eigenvectors,N_det,psi_det,N_int,&
         N_det,size(eigenvectors,1))
       do j=1,N_det
         ! Select at least n_states states with S^2 values closed to "expected_s2"
         if(dabs(s2_eigvalues(j)-expected_s2).le.0.5d0)then
           i_state +=1
           index_good_state_array(i_state) = j
           good_state_array(j) = .True.
         endif
         if(i_state.eq.N_states) then
           exit
         endif
       enddo
       if(i_state .ne.0)then
         ! Fill the first "i_state" states that have a correct S^2 value
         do j = 1, i_state
           do i=1,N_det
             CI_eigenvectors(i,j) = eigenvectors(i,index_good_state_array(j))
           enddo
           CI_electronic_energy(j) = eigenvalues(index_good_state_array(j))
           CI_eigenvectors_s2(j) = s2_eigvalues(index_good_state_array(j))
         enddo
         i_other_state = 0
         do j = 1, N_det
           if(good_state_array(j))cycle
           i_other_state +=1
           if(i_state+i_other_state.gt.n_states_diag)then
             exit
           endif
           do i=1,N_det
             CI_eigenvectors(i,i_state+i_other_state) = eigenvectors(i,j)
           enddo
           CI_electronic_energy(i_state+i_other_state) = eigenvalues(j)
           CI_eigenvectors_s2(i_state+i_other_state) = s2_eigvalues(i_state+i_other_state)
         enddo
         
       else
         print*,''
         print*,'!!!!!!!!   WARNING  !!!!!!!!!'
         print*,'  Within the ',N_det,'determinants selected'
         print*,'  and the ',N_states_diag,'states requested'
         print*,'  We did not find any state with S^2 values close to ',expected_s2
         print*,'  We will then set the first N_states eigenvectors of the H matrix'
         print*,'  as the CI_eigenvectors'
         print*,'  You should consider more states and maybe ask for s2_eig to be .True. or just enlarge the CI space'
         print*,''
         do j=1,min(N_states_diag,N_det)
           do i=1,N_det
             CI_eigenvectors(i,j) = eigenvectors(i,j)
           enddo
           CI_electronic_energy(j) = eigenvalues(j)
           CI_eigenvectors_s2(j) = s2_eigvalues(j)
         enddo
       endif
       deallocate(index_good_state_array,good_state_array)
       deallocate(s2_eigvalues)
     else
       call u_0_S2_u_0_nstates(CI_eigenvectors_s2,eigenvectors,N_det,psi_det,N_int,&
          min(N_det,N_states_diag),size(eigenvectors,1))
       ! Select the "N_states_diag" states of lowest energy
       do j=1,min(N_det,N_states_diag)
         do i=1,N_det
           CI_eigenvectors(i,j) = eigenvectors(i,j)
         enddo
         CI_electronic_energy(j) = eigenvalues(j)
       enddo
     endif
     deallocate(eigenvectors,eigenvalues)
   endif
   

   if( s2_eig.and.(n_states_diag > 1).and.(n_det >= n_states_diag) )then
      ! Diagonalizing S^2 within the "n_states_diag" states found
      allocate(s2_eigvalues(N_states_diag), e_array(N_states_diag))
      call diagonalize_s2_betweenstates(psi_det,CI_eigenvectors,n_det,size(psi_det,3),size(CI_eigenvectors,1),min(n_states_diag,n_det),s2_eigvalues)
      
      do j = 1, N_states_diag
        do i = 1, N_det
          psi_coef(i,j) = CI_eigenvectors(i,j)
        enddo
      enddo
      call u_0_H_u_0_nstates(e_array,psi_coef,n_det,psi_det,N_int,N_states_diag,psi_det_size)
     
      ! Browsing the "n_states_diag" states and getting the lowest in energy "n_states" ones that have the S^2 value
      ! closer to the "expected_s2" set as input
      
      allocate(index_good_state_array(N_det),good_state_array(N_det))
      good_state_array = .False.
      i_state = 0
      do j = 1, N_states_diag
        if(dabs(s2_eigvalues(j)-expected_s2).le.0.5d0)then
          good_state_array(j) = .True.
          i_state +=1
          index_good_state_array(i_state) = j
        endif
      enddo
      ! Sorting the i_state good states by energy
      allocate(iorder(i_state))
      do j = 1, i_state
        do i = 1, N_det
          CI_eigenvectors(i,j) = psi_coef(i,index_good_state_array(j))
        enddo
        CI_eigenvectors_s2(j) = s2_eigvalues(index_good_state_array(j))
        CI_electronic_energy(j) = e_array(j)
        iorder(j) = j
      enddo
      call dsort(e_array,iorder,i_state)
      do j = 1, i_state
        CI_electronic_energy(j) = e_array(j)
        CI_eigenvectors_s2(j) = s2_eigvalues(index_good_state_array(iorder(j)))
        do i = 1, N_det
          CI_eigenvectors(i,j) = psi_coef(i,index_good_state_array(iorder(j)))
        enddo
      enddo
      
      ! Then setting the other states without any specific energy order
      i_other_state = 0
      do j = 1, N_states_diag
        if(good_state_array(j))cycle
        i_other_state +=1
        do i = 1, N_det
          CI_eigenvectors(i,i_state + i_other_state) = psi_coef(i,j)
        enddo
        CI_eigenvectors_s2(i_state + i_other_state) = s2_eigvalues(j)
        CI_electronic_energy(i_state + i_other_state) = e_array(i_state + i_other_state)
      enddo
      deallocate(iorder,e_array,index_good_state_array,good_state_array)
       
     deallocate(s2_eigvalues)
     
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
