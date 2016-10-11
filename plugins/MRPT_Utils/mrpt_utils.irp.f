
 BEGIN_PROVIDER [ double precision, delta_ij, (N_det,N_det,N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_1h, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_1p, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_1h1p, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_2h, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_2p, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_1h2p, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_2h1p, (N_states) ]
&BEGIN_PROVIDER [ double precision, second_order_pt_new_2h2p, (N_states) ]
 implicit none
 BEGIN_DOC
 ! Dressing matrix in N_det basis
 END_DOC
 integer :: i,j,m
 integer :: i_state
 double precision :: accu(N_states)
 double precision, allocatable :: delta_ij_tmp(:,:,:)


 delta_ij = 0.d0

 allocate (delta_ij_tmp(N_det,N_det,N_states))


 ! 1h 
 delta_ij_tmp = 0.d0
 call H_apply_mrpt_1h(delta_ij_tmp,N_det)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
   second_order_pt_new_1h(i_state) = accu(i_state) 
 enddo
 print*, '1h   = ',accu
 
 ! 1p 
 delta_ij_tmp = 0.d0
 call H_apply_mrpt_1p(delta_ij_tmp,N_det)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_1p(i_state) = accu(i_state) 
 enddo
 print*, '1p   = ',accu

 ! 1h1p 
 delta_ij_tmp = 0.d0
 call H_apply_mrpt_1h1p(delta_ij_tmp,N_det)
 double precision :: e_corr_from_1h1p_singles(N_states)
!call give_singles_and_partial_doubles_1h1p_contrib(delta_ij_tmp,e_corr_from_1h1p_singles)
!call give_1h1p_only_doubles_spin_cross(delta_ij_tmp)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_1h1p(i_state) = accu(i_state) 
 enddo
 print*, '1h1p = ',accu

 ! 1h1p third order
 delta_ij_tmp = 0.d0
 call give_1h1p_sec_order_singles_contrib(delta_ij_tmp)
!call give_singles_and_partial_doubles_1h1p_contrib(delta_ij_tmp,e_corr_from_1h1p_singles)
!call give_1h1p_only_doubles_spin_cross(delta_ij_tmp)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_1h1p(i_state) = accu(i_state) 
 enddo
 print*, '1h1p(3)',accu

 ! 2h   
 delta_ij_tmp = 0.d0
 call H_apply_mrpt_2h(delta_ij_tmp,N_det)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_2h(i_state) = accu(i_state) 
 enddo
 print*, '2h   = ',accu

 ! 2p   
 delta_ij_tmp = 0.d0
 call H_apply_mrpt_2p(delta_ij_tmp,N_det)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_2p(i_state) = accu(i_state) 
 enddo
 print*, '2p   = ',accu

 ! 1h2p   
 delta_ij_tmp = 0.d0
!call give_1h2p_contrib(delta_ij_tmp)
 call H_apply_mrpt_1h2p(delta_ij_tmp,N_det)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_1h2p(i_state) = accu(i_state) 
 enddo
 print*, '1h2p = ',accu

 ! 2h1p   
 delta_ij_tmp = 0.d0
!call give_2h1p_contrib(delta_ij_tmp)
 call H_apply_mrpt_2h1p(delta_ij_tmp,N_det)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
  do j = 1, N_det
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
 second_order_pt_new_2h1p(i_state) = accu(i_state) 
 enddo
 print*, '2h1p = ',accu

 ! 2h2p   
!delta_ij_tmp = 0.d0
!call H_apply_mrpt_2h2p(delta_ij_tmp,N_det)
!accu = 0.d0
!do i_state = 1, N_states
!do i = 1, N_det
! do j = 1, N_det
!  accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
!  delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
! enddo
!enddo
!second_order_pt_new_2h2p(i_state) = accu(i_state) 
!enddo
!print*, '2h2p = ',accu

 double precision :: contrib_2h2p(N_states)
 call give_2h2p(contrib_2h2p)
 do i_state = 1, N_states
 do i = 1, N_det
   delta_ij(i,i,i_state) += contrib_2h2p(i_state)
 enddo
 second_order_pt_new_2h2p(i_state) = contrib_2h2p(i_state) 
 enddo
 print*, '2h2p = ',contrib_2h2p(1) 
 

 ! total  
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det
! write(*,'(1000(F16.10,x))')delta_ij(i,:,:)
  do j = i_state, N_det
   accu(i_state) += delta_ij(j,i,i_state) * psi_coef(i,i_state) * psi_coef(j,i_state)
  enddo
 enddo
 second_order_pt_new(i_state) = accu(i_state) 
 print*, 'total= ',accu(i_state)
 enddo




END_PROVIDER

 BEGIN_PROVIDER [double precision, Hmatrix_dressed_pt2_new, (N_det,N_det,N_states)]
 implicit none
 integer :: i,j,i_state
 do i_state = 1, N_states
  do i = 1,N_det
   do j = 1,N_det
    Hmatrix_dressed_pt2_new(j,i,i_state) = H_matrix_all_dets(j,i) + delta_ij(j,i,i_state)
   enddo
  enddo
 enddo
 END_PROVIDER 

 

 BEGIN_PROVIDER [double precision, Hmatrix_dressed_pt2_new_symmetrized, (N_det,N_det,N_states)]
 implicit none
 integer :: i,j,i_state
 do i_state = 1, N_states
  do i = 1,N_det
   do j = i,N_det
    Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state) =  H_matrix_all_dets(j,i) & 
                                            + 0.5d0 * ( delta_ij(j,i,i_state) + delta_ij(i,j,i_state) )
    Hmatrix_dressed_pt2_new_symmetrized(i,j,i_state) =  Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state) 
   enddo
  enddo
 enddo
 END_PROVIDER 

  BEGIN_PROVIDER [ double precision, CI_electronic_dressed_pt2_new_energy, (N_states_diag) ]
 &BEGIN_PROVIDER [ double precision, CI_dressed_pt2_new_eigenvectors, (N_det,N_states_diag) ]
 &BEGIN_PROVIDER [ double precision, CI_dressed_pt2_new_eigenvectors_s2, (N_states_diag) ]
  BEGIN_DOC
  ! Eigenvectors/values of the CI matrix
  END_DOC
  implicit none
  double precision :: ovrlp,u_dot_v
  integer :: i_good_state
  integer, allocatable  :: index_good_state_array(:)
  logical, allocatable  :: good_state_array(:)
  double precision, allocatable :: s2_values_tmp(:)
  integer :: i_other_state
  double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:)
  integer :: i_state
  double precision :: s2,e_0
  integer                        :: i,j,k
  double precision, allocatable :: s2_eigvalues(:)
  double precision, allocatable :: e_array(:)
  integer, allocatable :: iorder(:)
  
  ! Guess values for the "N_states_diag" states of the CI_dressed_pt2_new_eigenvectors 
  do j=1,min(N_states_diag,N_det)
    do i=1,N_det
      CI_dressed_pt2_new_eigenvectors(i,j) = psi_coef(i,j)
    enddo
  enddo

  do j=N_det+1,N_states_diag
    do i=1,N_det
      CI_dressed_pt2_new_eigenvectors(i,j) = 0.d0
    enddo
  enddo
  
  if (diag_algorithm == "Davidson") then
    
    print*, 'Davidson not yet implemented for the dressing ... '
    stop
    
  else if (diag_algorithm == "Lapack") then
    
    allocate (eigenvectors(size(H_matrix_all_dets,1),N_det))
    allocate (eigenvalues(N_det))
    call lapack_diag(eigenvalues,eigenvectors,                       &
        Hmatrix_dressed_pt2_new_symmetrized(1,1,1),N_det,N_det)
    CI_electronic_dressed_pt2_new_energy(:) = 0.d0
    if (s2_eig) then
      i_state = 0
      allocate (s2_eigvalues(N_det))
      allocate(index_good_state_array(N_det),good_state_array(N_det))
      good_state_array = .False.
      do j=1,N_det
        call get_s2_u0(psi_det,eigenvectors(1,j),N_det,size(eigenvectors,1),s2)
        s2_eigvalues(j) = s2
        ! Select at least n_states states with S^2 values closed to "expected_s2"
        if(dabs(s2-expected_s2).le.0.3d0)then
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
           CI_dressed_pt2_new_eigenvectors(i,j) = eigenvectors(i,index_good_state_array(j))
          enddo
          CI_electronic_dressed_pt2_new_energy(j) = eigenvalues(index_good_state_array(j))
          CI_dressed_pt2_new_eigenvectors_s2(j) = s2_eigvalues(index_good_state_array(j))
       enddo
       i_other_state = 0
       do j = 1, N_det 
        if(good_state_array(j))cycle
        i_other_state +=1
        if(i_state+i_other_state.gt.n_states_diag)then
         exit
        endif
        call get_s2_u0(psi_det,eigenvectors(1,j),N_det,size(eigenvectors,1),s2)
        do i=1,N_det
         CI_dressed_pt2_new_eigenvectors(i,i_state+i_other_state) = eigenvectors(i,j)
        enddo
        CI_electronic_dressed_pt2_new_energy(i_state+i_other_state) = eigenvalues(j)
        CI_dressed_pt2_new_eigenvectors_s2(i_state+i_other_state) = s2
       enddo
 
       deallocate(index_good_state_array,good_state_array)

      else
       print*,''
       print*,'!!!!!!!!   WARNING  !!!!!!!!!'
       print*,'  Within the ',N_det,'determinants selected'
       print*,'  and the ',N_states_diag,'states requested'
       print*,'  We did not find any state with S^2 values close to ',expected_s2
       print*,'  We will then set the first N_states eigenvectors of the H matrix'
       print*,'  as the CI_dressed_pt2_new_eigenvectors'
       print*,'  You should consider more states and maybe ask for diagonalize_s2 to be .True. or just enlarge the CI space'
       print*,''
       do j=1,min(N_states_diag,N_det)
         do i=1,N_det
           CI_dressed_pt2_new_eigenvectors(i,j) = eigenvectors(i,j)
         enddo
        CI_electronic_dressed_pt2_new_energy(j) = eigenvalues(j)
        CI_dressed_pt2_new_eigenvectors_s2(j) = s2_eigvalues(j)
       enddo
      endif
     deallocate(s2_eigvalues)
    else
      ! Select the "N_states_diag" states of lowest energy
      do j=1,min(N_det,N_states_diag)
        call get_s2_u0(psi_det,eigenvectors(1,j),N_det,N_det,s2)
        do i=1,N_det
          CI_dressed_pt2_new_eigenvectors(i,j) = eigenvectors(i,j)
        enddo
        CI_electronic_dressed_pt2_new_energy(j) = eigenvalues(j)
        CI_dressed_pt2_new_eigenvectors_s2(j) = s2
      enddo
    endif
    deallocate(eigenvectors,eigenvalues)
  endif

 
END_PROVIDER
 

BEGIN_PROVIDER [ double precision, CI_dressed_pt2_new_energy, (N_states_diag) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_determinants)
  do j=1,N_states_diag
    CI_dressed_pt2_new_energy(j) = CI_electronic_dressed_pt2_new_energy(j) + nuclear_repulsion
    write(st,'(I4)') j
    call write_double(output_determinants,CI_dressed_pt2_new_energy(j),'Energy of state '//trim(st))
    call write_double(output_determinants,CI_eigenvectors_s2(j),'S^2 of state '//trim(st))
  enddo

END_PROVIDER
