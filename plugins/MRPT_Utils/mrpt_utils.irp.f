
 BEGIN_PROVIDER [ double precision, delta_ij, (N_det_ref,N_det_ref,N_states) ]
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
 ! Dressing matrix in N_det_ref basis
 END_DOC
 integer :: i,j,m
 integer :: i_state
 double precision :: accu(N_states)
 double precision, allocatable :: delta_ij_tmp(:,:,:)


 delta_ij = 0.d0

 allocate (delta_ij_tmp(N_det_ref,N_det_ref,N_states))


 ! 1h 
 delta_ij_tmp = 0.d0
 call H_apply_mrpt_1h(delta_ij_tmp,N_det_ref)
 accu = 0.d0
 do i_state = 1, N_states
 do i = 1, N_det_ref
   write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
  do j = 1, N_det_ref
   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  enddo
 enddo
   second_order_pt_new_1h(i_state) = accu(i_state) 
 enddo
 print*, '1h   = ',accu
 
   ! 1p 
   delta_ij_tmp = 0.d0
   call H_apply_mrpt_1p(delta_ij_tmp,N_det_ref)
   accu = 0.d0
   do i_state = 1, N_states
   do i = 1, N_det_ref
     write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
    do j = 1, N_det_ref
!    print*, accu
!    print*,delta_ij_tmp(j,i,i_state) , psi_ref_coef(i,i_state) , psi_ref_coef(j,i_state)
     accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
     delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
    enddo
   enddo
   second_order_pt_new_1p(i_state) = accu(i_state) 
   enddo
   print*, '1p   = ',accu
  
   ! 1h1p 
   delta_ij_tmp = 0.d0
   call H_apply_mrpt_1h1p(delta_ij_tmp,N_det_ref)
   accu = 0.d0
   do i_state = 1, N_states
   do i = 1, N_det_ref
     write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
    do j = 1, N_det_ref
     accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
     delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
    enddo
   enddo
   double precision :: accu_diag,accu_non_diag
   accu_diag = 0.d0 
   accu_non_diag = 0.d0
   do i = 1, N_det_ref
    accu_diag += delta_ij_tmp(i,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(i,i_state)
    do j = 1, N_det_ref
     if(i == j)cycle
     accu_non_diag += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
    enddo 
   enddo
   second_order_pt_new_1h1p(i_state) = accu(i_state) 
   enddo
  !double precision :: neutral, ionic
  !neutral = 0.d0
  !do i = 1, 2 
  ! do j = 1, N_det_ref
  !  neutral += psi_ref_coef(j,1) * delta_ij_tmp(j,i,1) * psi_ref_coef(i,1)
  ! enddo
  !enddo
  !do i = 3, 4 
  ! do j = 1, N_det_ref
  !  ionic += psi_ref_coef(j,1) * delta_ij_tmp(j,i,1) * psi_ref_coef(i,1)
  ! enddo
  !enddo
  !neutral = delta_ij_tmp(1,1,1) * psi_ref_coef(1,1)**2 + delta_ij_tmp(2,2,1) * psi_ref_coef(2,1)**2 & 
  !        + delta_ij_tmp(1,2,1) * psi_ref_coef(1,1)* psi_ref_coef(2,1) + delta_ij_tmp(2,1,1) * psi_ref_coef(1,1)* psi_ref_coef(2,1)
  !ionic   = delta_ij_tmp(3,3,1) * psi_ref_coef(3,1)**2 + delta_ij_tmp(4,4,1) * psi_ref_coef(4,1)**2 & 
  !        + delta_ij_tmp(3,4,1) * psi_ref_coef(3,1)* psi_ref_coef(4,1) + delta_ij_tmp(4,3,1) * psi_ref_coef(3,1)* psi_ref_coef(4,1)
  !neutral = delta_ij_tmp(1,1,1) 
  !ionic   = delta_ij_tmp(3,3,1) 
  !print*, 'neutral = ',neutral
  !print*, 'ionic   = ',ionic
   print*, '1h1p = ',accu
  
  !! 1h1p third order
  !if(do_third_order_1h1p)then
  ! delta_ij_tmp = 0.d0
  ! call give_1h1p_sec_order_singles_contrib(delta_ij_tmp)
  ! accu = 0.d0
  ! do i_state = 1, N_states
  ! do i = 1, N_det_ref
  !  write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
  !  do j = 1, N_det_ref
  !   accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
  !   delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
  !  enddo
  ! enddo
  ! second_order_pt_new_1h1p(i_state) = accu(i_state) 
  ! enddo
  ! print*, '1h1p(3)',accu
  !endif
  
   ! 2h   
   delta_ij_tmp = 0.d0
   call H_apply_mrpt_2h(delta_ij_tmp,N_det_ref)
   accu = 0.d0
   do i_state = 1, N_states
   do i = 1, N_det_ref
     write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
    do j = 1, N_det_ref
     accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
     delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
    enddo
   enddo
   second_order_pt_new_2h(i_state) = accu(i_state) 
   enddo
   print*, '2h   = ',accu
  
   ! 2p   
   delta_ij_tmp = 0.d0
   call H_apply_mrpt_2p(delta_ij_tmp,N_det_ref)
   accu = 0.d0
   do i_state = 1, N_states
   do i = 1, N_det_ref
     write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
    do j = 1, N_det_ref
     accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
     delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
    enddo
   enddo
   second_order_pt_new_2p(i_state) = accu(i_state) 
   enddo
   print*, '2p   = ',accu
  
   ! 1h2p   
   delta_ij_tmp = 0.d0
   call give_1h2p_contrib(delta_ij_tmp)
   !!!call H_apply_mrpt_1h2p(delta_ij_tmp,N_det_ref)
   accu = 0.d0
   do i_state = 1, N_states
   do i = 1, N_det_ref
     write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
    do j = 1, N_det_ref
     accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
     delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
    enddo
   enddo
   second_order_pt_new_1h2p(i_state) = accu(i_state) 
   enddo
   print*, '1h2p = ',accu
  
   ! 2h1p   
   delta_ij_tmp = 0.d0
   call give_2h1p_contrib(delta_ij_tmp)
   !!!!call H_apply_mrpt_2h1p(delta_ij_tmp,N_det_ref)
   accu = 0.d0
   do i_state = 1, N_states
   do i = 1, N_det_ref
     write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
    do j = 1, N_det_ref
     accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
     delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
    enddo
   enddo
   second_order_pt_new_2h1p(i_state) = accu(i_state) 
   enddo
   print*, '2h1p = ',accu
  
   ! 2h2p   
  
   double precision :: contrib_2h2p(N_states)
   call give_2h2p(contrib_2h2p)
   do i_state = 1, N_states
    do i = 1, N_det_ref
      delta_ij(i,i,i_state) += contrib_2h2p(i_state)
    enddo
   second_order_pt_new_2h2p(i_state) = contrib_2h2p(i_state) 
   enddo
   print*, '2h2p = ',contrib_2h2p

!  ! 2h2p   old fashion
!  delta_ij_tmp = 0.d0
!  call H_apply_mrpt_2h2p(delta_ij_tmp,N_det_ref)
!  accu = 0.d0
!  do i_state = 1, N_states
!  do i = 1, N_det_ref
!    write(*,'(1000(F16.10,x))')delta_ij_tmp(i,:,i_state)
!   do j = 1, N_det_ref
!    accu(i_state) += delta_ij_tmp(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
!    delta_ij(j,i,i_state) += delta_ij_tmp(j,i,i_state)
!   enddo
!  enddo
!  second_order_pt_new_2h2p(i_state) = accu(i_state) 
!  enddo
!  print*, '2h2p   = ',accu
   

 ! total  
 accu = 0.d0
 print*, 'naked matrix'
 double precision, allocatable :: hmatrix(:,:)
 double precision:: hij,h00
 allocate(hmatrix(N_det_ref, N_det_ref))
 call i_h_j(psi_ref(1,1,1),psi_ref(1,1,1),N_int,h00)
 do i = 1, N_det_ref
  do j = 1, N_det_Ref
   call i_h_j(psi_ref(1,1,i),psi_ref(1,1,j),N_int,hij)
   hmatrix(i,j) = hij 
  enddo
  hmatrix(i,i) += - h00
 enddo
 do i = 1, N_det_ref
  write(*,'(1000(F16.10,x))')hmatrix(i,:)
 enddo
 print*, ''
 print*, ''
 print*, ''
 do i_state = 1, N_states
  print*,'state  ',i_state
  do i = 1, N_det_ref
   do j = 1, N_det_Ref
    call i_h_j(psi_ref(1,1,i),psi_ref(1,1,j),N_int,hij)
    hmatrix(i,j) = hij 
   enddo
   hmatrix(i,i) += - h00
  enddo
  do i = 1, N_det_ref
   write(*,'(1000(F16.10,x))')delta_ij(i,:,i_state)
   do j = 1 , N_det_ref
    accu(i_state) += delta_ij(j,i,i_state) * psi_ref_coef(i,i_state) * psi_ref_coef(j,i_state)
    hmatrix(i,j)  += delta_ij(j,i,i_state)
   enddo
  enddo
  second_order_pt_new(i_state) = accu(i_state) 
  print*, 'total= ',accu(i_state)

  do i = 1, N_det_ref
   write(*,'(1000(F16.10,x))')hmatrix(i,:)
  enddo
  
 enddo
 deallocate(hmatrix)




END_PROVIDER

 BEGIN_PROVIDER [double precision, Hmatrix_dressed_pt2_new, (N_det_ref,N_det_ref,N_states)]
 implicit none
 integer :: i,j,i_state
 double precision :: hij
 do i_state = 1, N_states
  do i = 1,N_det_ref
   do j = 1,N_det_ref
    call i_h_j(psi_ref(1,1,j),psi_ref(1,1,i),N_int,hij) 
    Hmatrix_dressed_pt2_new(j,i,i_state) = hij + delta_ij(j,i,i_state)
   enddo
  enddo
 enddo
 END_PROVIDER 

 

 BEGIN_PROVIDER [double precision, Hmatrix_dressed_pt2_new_symmetrized, (N_det_ref,N_det_ref,N_states)]
 implicit none
 integer :: i,j,i_state
 double precision :: hij
 double precision :: accu(N_states)
 accu = 0.d0
 do i_state = 1, N_states
  do i = 1,N_det_ref
   do j = 1,N_det_ref
    call i_h_j(psi_ref(1,1,j),psi_ref(1,1,i),N_int,hij) 
    Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state) =  hij & 
                                            + 0.5d0 * ( delta_ij(j,i,i_state) + delta_ij(i,j,i_state) )
!   Hmatrix_dressed_pt2_new_symmetrized(i,j,i_state) =  Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state) 
    accu(i_State) += psi_ref_coef(i,i_State) * Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state) * psi_ref_coef(j,i_State) 
   enddo
  enddo
 enddo
 print*, 'accu = ',accu + nuclear_repulsion
 END_PROVIDER 

  BEGIN_PROVIDER [ double precision, CI_electronic_dressed_pt2_new_energy, (N_states_diag_heff) ]
 &BEGIN_PROVIDER [ double precision, CI_dressed_pt2_new_eigenvectors, (N_det_ref,N_states_diag_heff) ]
 &BEGIN_PROVIDER [ double precision, CI_dressed_pt2_new_eigenvectors_s2, (N_states_diag_heff) ]
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
  double precision, allocatable  :: eigenvectors(:,:), eigenvalues(:), hmatrix_tmp(:,:)
  integer :: i_state
  double precision :: s2,e_0
  integer                        :: i,j,k
  double precision, allocatable :: s2_eigvalues(:)
  double precision, allocatable :: e_array(:)
  integer, allocatable :: iorder(:)
  double precision :: overlap(N_det_ref)
  double precision, allocatable :: psi_tmp(:)
  
  ! Guess values for the "N_states_diag_heff" states of the CI_dressed_pt2_new_eigenvectors 
  do j=1,min(N_states,N_det_ref)
    do i=1,N_det_ref
      CI_dressed_pt2_new_eigenvectors(i,j) = psi_ref_coef(i,j)
    enddo
  enddo

  do j=min(N_states,N_det_ref)+1,N_states_diag_heff
    do i=1,N_det_ref
      CI_dressed_pt2_new_eigenvectors(i,j) = 0.d0
    enddo
  enddo
  
  if (diag_algorithm == "Davidson") then
    
    print*, 'Davidson not yet implemented for the dressing ... '
    stop
    
  else if (diag_algorithm == "Lapack") then
     allocate (eigenvectors(N_det_ref,N_det_ref))
     allocate (eigenvalues(N_det_ref))
     if(pure_state_specific_mrpt2)then
      allocate (hmatrix_tmp(N_det_ref,N_det_ref))
      allocate (iorder(N_det_ref))
      allocate (psi_tmp(N_det_ref))
      print*,''
      print*,'***************************'
      do i_state = 1, N_states !! Big loop over states
       print*,''
       print*,'Diagonalizing with the dressing for state',i_state
       do i = 1, N_det_ref
        do j = 1, N_det_ref
         hmatrix_tmp(j,i) = Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state)
        enddo
!       print*,i,hmatrix_tmp(i,i)+nuclear_repulsion
       enddo
       call lapack_diag(eigenvalues,eigenvectors,hmatrix_tmp,N_det_ref,N_det_ref)
       write(*,'(A86)')'Looking for the most overlapping state within all eigenvectors of the dressed matrix'
       print*,''
       print*,'Calculating the overlap for ...'
       do i = 1, N_det_ref
        overlap(i) = 0.d0
        iorder(i) = i
        print*,'eigenvector',i
        do j = 1, N_det_ref
         overlap(i)+= psi_ref_coef(j,i_state) * eigenvectors(j,i)
        enddo
        overlap(i) = -dabs(overlap(i))
        print*,'energy  = ',eigenvalues(i) + nuclear_repulsion
        print*,'overlap = ',dabs(overlap(i))
       enddo
       print*,''
       print*,'Sorting the eigenvectors per overlap'
       call dsort(overlap,iorder,n_det_ref)
       do j = 1, N_det_ref
        print*,overlap(j),iorder(j)
       enddo
       print*,''
       print*,'The most overlapping state is the ',iorder(1)
       print*,'with the overlap of ',dabs(overlap(1))
       print*,'and an energy of    ',eigenvalues(iorder(1)) + nuclear_repulsion
       print*,'Calculating the S^2 value ...'
       do i=1,N_det_ref
         CI_dressed_pt2_new_eigenvectors(i,i_state) = eigenvectors(i,iorder(1))
         psi_tmp(i) = eigenvectors(i,iorder(1))
       enddo
       CI_electronic_dressed_pt2_new_energy(i_state) = eigenvalues(iorder(1))
       call u_0_S2_u_0(CI_dressed_pt2_new_eigenvectors_s2(i_state),psi_tmp,N_det_ref,psi_det,N_int,1,N_det_ref)
       print*,'S^2      = ', CI_dressed_pt2_new_eigenvectors_s2(i_state)
      enddo
    !else if(state_average)then
    !  print*,''
    !  print*,'***************************'
    !  print*,''
    !  print*,'Doing state average dressings'
    ! allocate (hmatrix_tmp(N_det_ref,N_det_ref))
    ! hmatrix_tmp = 0.d0
    ! do i_state = 1, N_states !! Big loop over states
    !  do i = 1, N_det_ref
    !   do j = 1, N_det_ref
    !    hmatrix_tmp(j,i) += Hmatrix_dressed_pt2_new_symmetrized(j,i,i_state)
    !   enddo
    !  enddo
    ! enddo


    ! deallocate(hmatrix_tmp)

     else 

      call lapack_diag(eigenvalues,eigenvectors,                      &
          Hmatrix_dressed_pt2_new_symmetrized(1,1,1),N_det_ref,N_det_ref)
      CI_electronic_dressed_pt2_new_energy(:) = 0.d0
      if (s2_eig) then
        i_state = 0
        allocate (s2_eigvalues(N_det_ref))
        allocate(index_good_state_array(N_det_ref),good_state_array(N_det_ref))
        good_state_array = .False.
        call u_0_S2_u_0(s2_eigvalues,eigenvectors,N_det_ref,psi_det,N_int,&
          N_det_ref,size(eigenvectors,1))
        do j=1,N_det_ref
          ! Select at least n_states states with S^2 values closed to "expected_s2"
          print*, eigenvalues(j)+nuclear_repulsion, s2_eigvalues(j)
          if(dabs(s2_eigvalues(j)-expected_s2).le.0.5d0)then
            i_state += 1
            index_good_state_array(i_state) = j
            good_state_array(j) = .True.
          endif
          if (i_state==N_states) then
            exit
          endif
        enddo
        if (i_state /= 0) then
          ! Fill the first "i_state" states that have a correct S^2 value
          do j = 1, i_state
            do i=1,N_det_ref
              CI_dressed_pt2_new_eigenvectors(i,j) = eigenvectors(i,index_good_state_array(j))
            enddo
            CI_electronic_dressed_pt2_new_energy(j) = eigenvalues(index_good_state_array(j))
            CI_dressed_pt2_new_eigenvectors_s2(j) = s2_eigvalues(index_good_state_array(j))
          enddo
          i_other_state = 0
          do j = 1, N_det_ref
            if(good_state_array(j))cycle
            i_other_state +=1
            if(i_state+i_other_state.gt.n_states)then
              exit
            endif
            do i=1,N_det_ref
              CI_dressed_pt2_new_eigenvectors(i,i_state+i_other_state) = eigenvectors(i,j)
            enddo
            CI_electronic_dressed_pt2_new_energy(i_state+i_other_state) = eigenvalues(j)
            CI_dressed_pt2_new_eigenvectors_s2(i_state+i_other_state) = s2_eigvalues(i_state+i_other_state)
          enddo
          
        else
          print*,''
          print*,'!!!!!!!!   WARNING  !!!!!!!!!'
          print*,'  Within the ',N_det_ref,'determinants selected'
          print*,'  and the ',N_states_diag_heff,'states requested'
          print*,'  We did not find any state with S^2 values close to ',expected_s2
          print*,'  We will then set the first N_states eigenvectors of the H matrix'
          print*,'  as the CI_dressed_pt2_new_eigenvectors'
          print*,'  You should consider more states and maybe ask for s2_eig to be .True. or just enlarge the CI space'
          print*,''
          do j=1,min(N_states_diag_heff,N_det_ref)
            do i=1,N_det_ref
              CI_dressed_pt2_new_eigenvectors(i,j) = eigenvectors(i,j)
            enddo
            CI_electronic_dressed_pt2_new_energy(j) = eigenvalues(j)
            CI_dressed_pt2_new_eigenvectors_s2(j) = s2_eigvalues(j)
          enddo
        endif
        deallocate(index_good_state_array,good_state_array)
        deallocate(s2_eigvalues)
      else
        call u_0_S2_u_0(CI_dressed_pt2_new_eigenvectors_s2,eigenvectors,N_det_ref,psi_det,N_int,&
           min(N_det_ref,N_states_diag_heff),size(eigenvectors,1))
        ! Select the "N_states_diag_heff" states of lowest energy
        do j=1,min(N_det_ref,N_states)
          do i=1,N_det_ref
            CI_dressed_pt2_new_eigenvectors(i,j) = eigenvectors(i,j)
          enddo
          CI_electronic_dressed_pt2_new_energy(j) = eigenvalues(j)
        enddo
      endif
      deallocate(eigenvectors,eigenvalues)
     endif

  endif

 
END_PROVIDER
 

BEGIN_PROVIDER [ double precision, CI_dressed_pt2_new_energy, (N_states_diag_heff) ]
  implicit none
  BEGIN_DOC
  ! N_states lowest eigenvalues of the CI matrix
  END_DOC
  
  integer                        :: j
  character*(8)                  :: st
  call write_time(output_determinants)
  do j=1,N_states_diag_heff
    CI_dressed_pt2_new_energy(j) = CI_electronic_dressed_pt2_new_energy(j) + nuclear_repulsion
    write(st,'(I4)') j
    call write_double(output_determinants,CI_dressed_pt2_new_energy(j),'Energy of state '//trim(st))
    call write_double(output_determinants, CI_dressed_pt2_new_eigenvectors_s2(j) ,'S^2 of state '//trim(st))
  enddo

END_PROVIDER
