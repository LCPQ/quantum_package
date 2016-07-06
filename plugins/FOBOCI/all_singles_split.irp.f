subroutine all_single_split(psi_det_generators_input,psi_coef_generators_input,Ndet_generators_input,dressing_matrix)
 implicit none
 use bitmasks
 integer, intent(in) :: Ndet_generators_input
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,Ndet_generators_input)
 double precision, intent(inout) :: dressing_matrix(Ndet_generators_input,Ndet_generators_input)
 double precision, intent(in)    :: psi_coef_generators_input(ndet_generators_input,n_states)
 integer :: i,i_hole,j
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi
 do i = 1, n_inact_orb
   i_hole = list_inact(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call set_psi_det_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call all_single
!  call diagonalize_CI_SC2
!  call update_matrix_dressing_sc2(dressing_matrix,ndet_generators_input,psi_det_generators_input,Diag_H_elements_SC2)
   call provide_matrix_dressing(dressing_matrix,ndet_generators_input,psi_det_generators_input)
 enddo

 do i = 1, n_act_orb
   i_hole = list_act(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call set_psi_det_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call all_single
!  call diagonalize_CI_SC2
!  call update_matrix_dressing_sc2(dressing_matrix,ndet_generators_input,psi_det_generators_input,Diag_H_elements_SC2)
   call provide_matrix_dressing(dressing_matrix,ndet_generators_input,psi_det_generators_input)
 enddo

 do i = 1, n_virt_orb
   i_hole = list_virt(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call set_psi_det_as_input_psi(ndet_generators_input,psi_det_generators_input,psi_coef_generators_input)
   call all_single
!  call diagonalize_CI_SC2
!  call update_matrix_dressing_sc2(dressing_matrix,ndet_generators_input,psi_det_generators_input,Diag_H_elements_SC2)
   call provide_matrix_dressing(dressing_matrix,ndet_generators_input,psi_det_generators_input)
 enddo

 n_det_max_jacobi = 1000
 soft_touch n_det_max_jacobi
end



subroutine all_single_for_1p(i_particl,dressing_matrix_1h1p,dressing_matrix_1h2p,dressing_matrix_extra_1h_or_1p)
 implicit none
 use bitmasks
 integer, intent(in)             :: i_particl
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_1h2p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_extra_1h_or_1p(N_det_generators,N_det_generators)
 integer :: i,j
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 call all_single
 
 threshold_davidson = 1.d-12
 soft_touch threshold_davidson davidson_criterion
 call diagonalize_CI



 double precision, allocatable  :: matrix_ref_1h_1p(:,:)
 double precision, allocatable  :: matrix_ref_1h_1p_dressing_1h1p(:,:)
 double precision, allocatable  :: matrix_ref_1h_1p_dressing_1h2p(:,:)
 double precision, allocatable  :: psi_coef_ref_1h_1p(:,:)
 double precision, allocatable  :: psi_coef_1h1p(:,:)
 double precision, allocatable  :: psi_coef_1h2p(:,:)
 integer(bit_kind), allocatable :: psi_det_1h2p(:,:,:)
 integer(bit_kind), allocatable :: psi_det_ref_1h_1p(:,:,:)
 integer(bit_kind), allocatable :: psi_det_1h1p(:,:,:)
 integer :: n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p
 double precision :: hka
 double precision,allocatable :: eigenvectors(:,:), eigenvalues(:)


 call give_n_ref_1h_1p_and_n_1h2p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p)

 allocate(matrix_ref_1h_1p(n_det_ref_1h_1p,n_det_ref_1h_1p))
 allocate(matrix_ref_1h_1p_dressing_1h1p(n_det_ref_1h_1p,n_det_ref_1h_1p))
 allocate(matrix_ref_1h_1p_dressing_1h2p(n_det_ref_1h_1p,n_det_ref_1h_1p))
 allocate(psi_det_ref_1h_1p(N_int,2,n_det_ref_1h_1p), psi_coef_ref_1h_1p(n_det_ref_1h_1p,N_states))
 allocate(psi_det_1h2p(N_int,2,n_det_1h2p), psi_coef_1h2p(n_det_1h2p,N_states))
 allocate(psi_det_1h1p(N_int,2,n_det_1h1p), psi_coef_1h1p(n_det_1h1p,N_states))

 call give_wf_n_ref_1h_1p_and_n_1h2p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,&
                                                          psi_det_1h2p,psi_coef_1h2p,psi_det_1h1p,psi_coef_1h1p)

 do i = 1, n_det_ref_1h_1p
  do j = 1, n_det_ref_1h_1p
   call i_h_j(psi_det_ref_1h_1p(1,1,i),psi_det_ref_1h_1p(1,1,j),N_int,hka)
   matrix_ref_1h_1p(i,j) = hka
  enddo
 enddo
 matrix_ref_1h_1p_dressing_1h1p = 0.d0
 matrix_ref_1h_1p_dressing_1h2p = 0.d0
 call provide_matrix_dressing_general(matrix_ref_1h_1p_dressing_1h2p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,n_det_ref_1h_1p, &
                                                        psi_det_1h2p,psi_coef_1h2p,n_det_1h2p)
 call provide_matrix_dressing_general(matrix_ref_1h_1p_dressing_1h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,n_det_ref_1h_1p, &
                                                        psi_det_1h1p,psi_coef_1h1p,n_det_1h1p)
 do i = 1, n_det_ref_1h_1p
  do j = 1, n_det_ref_1h_1p
   matrix_ref_1h_1p(i,j) += matrix_ref_1h_1p_dressing_1h2p(i,j) + matrix_ref_1h_1p_dressing_1h1p(i,j)
  enddo
 enddo
 
 allocate(eigenvectors(n_det_ref_1h_1p,n_det_ref_1h_1p), eigenvalues(n_det_ref_1h_1p))
 call lapack_diag(eigenvalues,eigenvectors,matrix_ref_1h_1p,n_det_ref_1h_1p,n_det_ref_1h_1p)
!do j = 1, n_det_ref_1h_1p
! print*,'coef = ',eigenvectors(j,1)
!enddo
 print*,''
 print*,'-----------------------'
 print*,'-----------------------'
 print*,'e_dressed = ',eigenvalues(1)+nuclear_repulsion
 print*,'-----------------------'
 ! Extract the 
 integer, allocatable :: index_generator(:)
 integer :: n_det_generators_tmp,degree
 n_det_generators_tmp = 0
 allocate(index_generator(n_det_ref_1h_1p))
 do i = 1, n_det_ref_1h_1p
  do j = 1, N_det_generators
   call get_excitation_degree(psi_det_generators(1,1,j),psi_det_ref_1h_1p(1,1,i), degree, N_int)
   if(degree == 0)then
    n_det_generators_tmp +=1
    index_generator(n_det_generators_tmp) = i
   endif
  enddo
 enddo
 if(n_det_generators_tmp .ne. n_det_generators)then
  print*,'PB !!!'
  print*,'if(n_det_generators_tmp .ne. n_det_genrators)then'
  stop
 endif
 do i = 1, N_det_generators
  print*,'psi_coef_dressed = ',eigenvectors(index_generator(i),1)
  do j = 1, N_det_generators
   dressing_matrix_1h1p(i,j) += matrix_ref_1h_1p_dressing_1h1p(index_generator(i),index_generator(j))
   dressing_matrix_1h2p(i,j) += matrix_ref_1h_1p_dressing_1h2p(index_generator(i),index_generator(j))
  enddo
 enddo
 print*,'-----------------------'
 print*,'-----------------------'
 

 deallocate(matrix_ref_1h_1p)
 deallocate(matrix_ref_1h_1p_dressing_1h1p)
 deallocate(matrix_ref_1h_1p_dressing_1h2p)
 deallocate(psi_det_ref_1h_1p, psi_coef_ref_1h_1p)
 deallocate(psi_det_1h2p, psi_coef_1h2p)
 deallocate(psi_det_1h1p, psi_coef_1h1p)
 deallocate(eigenvectors,eigenvalues)
 deallocate(index_generator)


end

subroutine all_single_for_1h(i_hole,dressing_matrix_1h1p,dressing_matrix_2h1p,dressing_matrix_extra_1h_or_1p)
 implicit none
 use bitmasks
 integer, intent(in)             :: i_hole
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_2h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_extra_1h_or_1p(N_det_generators,N_det_generators)
 integer :: i,j
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 call all_single
 
 threshold_davidson = 1.d-12
 soft_touch threshold_davidson davidson_criterion
 call diagonalize_CI



 double precision, allocatable  :: matrix_ref_1h_1p(:,:)
 double precision, allocatable  :: matrix_ref_1h_1p_dressing_1h1p(:,:)
 double precision, allocatable  :: matrix_ref_1h_1p_dressing_2h1p(:,:)
 double precision, allocatable  :: psi_coef_ref_1h_1p(:,:)
 double precision, allocatable  :: psi_coef_1h1p(:,:)
 double precision, allocatable  :: psi_coef_2h1p(:,:)
 integer(bit_kind), allocatable :: psi_det_2h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_det_ref_1h_1p(:,:,:)
 integer(bit_kind), allocatable :: psi_det_1h1p(:,:,:)
 integer :: n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p
 double precision :: hka
 double precision,allocatable :: eigenvectors(:,:), eigenvalues(:)


 call give_n_ref_1h_1p_and_n_2h1p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p)

 allocate(matrix_ref_1h_1p(n_det_ref_1h_1p,n_det_ref_1h_1p))
 allocate(matrix_ref_1h_1p_dressing_1h1p(n_det_ref_1h_1p,n_det_ref_1h_1p))
 allocate(matrix_ref_1h_1p_dressing_2h1p(n_det_ref_1h_1p,n_det_ref_1h_1p))
 allocate(psi_det_ref_1h_1p(N_int,2,n_det_ref_1h_1p), psi_coef_ref_1h_1p(n_det_ref_1h_1p,N_states))
 allocate(psi_det_2h1p(N_int,2,n_det_2h1p), psi_coef_2h1p(n_det_2h1p,N_states))
 allocate(psi_det_1h1p(N_int,2,n_det_1h1p), psi_coef_1h1p(n_det_1h1p,N_states))

 call give_wf_n_ref_1h_1p_and_n_2h1p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,&
                                                          psi_det_2h1p,psi_coef_2h1p,psi_det_1h1p,psi_coef_1h1p)

 do i = 1, n_det_ref_1h_1p
  do j = 1, n_det_ref_1h_1p
   call i_h_j(psi_det_ref_1h_1p(1,1,i),psi_det_ref_1h_1p(1,1,j),N_int,hka)
   matrix_ref_1h_1p(i,j) = hka
  enddo
 enddo
 matrix_ref_1h_1p_dressing_1h1p = 0.d0
 matrix_ref_1h_1p_dressing_2h1p = 0.d0
 call provide_matrix_dressing_general(matrix_ref_1h_1p_dressing_2h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,n_det_ref_1h_1p, &
                                                        psi_det_2h1p,psi_coef_2h1p,n_det_2h1p)
 call provide_matrix_dressing_general(matrix_ref_1h_1p_dressing_1h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,n_det_ref_1h_1p, &
                                                        psi_det_1h1p,psi_coef_1h1p,n_det_1h1p)
 do i = 1, n_det_ref_1h_1p
  do j = 1, n_det_ref_1h_1p
   matrix_ref_1h_1p(i,j) += matrix_ref_1h_1p_dressing_2h1p(i,j) + matrix_ref_1h_1p_dressing_1h1p(i,j)
  enddo
 enddo
 
 allocate(eigenvectors(n_det_ref_1h_1p,n_det_ref_1h_1p), eigenvalues(n_det_ref_1h_1p))
 call lapack_diag(eigenvalues,eigenvectors,matrix_ref_1h_1p,n_det_ref_1h_1p,n_det_ref_1h_1p)
!do j = 1, n_det_ref_1h_1p
! print*,'coef = ',eigenvectors(j,1)
!enddo
 print*,''
 print*,'-----------------------'
 print*,'-----------------------'
 print*,'e_dressed = ',eigenvalues(1)+nuclear_repulsion
 print*,'-----------------------'
 ! Extract the 
 integer, allocatable :: index_generator(:)
 integer :: n_det_generators_tmp,degree
 n_det_generators_tmp = 0
 allocate(index_generator(n_det_ref_1h_1p))
 do i = 1, n_det_ref_1h_1p
  do j = 1, N_det_generators
   call get_excitation_degree(psi_det_generators(1,1,j),psi_det_ref_1h_1p(1,1,i), degree, N_int)
   if(degree == 0)then
    n_det_generators_tmp +=1
    index_generator(n_det_generators_tmp) = i
   endif
  enddo
 enddo
 if(n_det_generators_tmp .ne. n_det_generators)then
  print*,'PB !!!'
  print*,'if(n_det_generators_tmp .ne. n_det_genrators)then'
  stop
 endif
 do i = 1, N_det_generators
  print*,'psi_coef_dressed = ',eigenvectors(index_generator(i),1)
  do j = 1, N_det_generators
   dressing_matrix_1h1p(i,j) += matrix_ref_1h_1p_dressing_1h1p(index_generator(i),index_generator(j))
   dressing_matrix_2h1p(i,j) += matrix_ref_1h_1p_dressing_2h1p(index_generator(i),index_generator(j))
  enddo
 enddo
 print*,'-----------------------'
 print*,'-----------------------'
 

 deallocate(matrix_ref_1h_1p)
 deallocate(matrix_ref_1h_1p_dressing_1h1p)
 deallocate(matrix_ref_1h_1p_dressing_2h1p)
 deallocate(psi_det_ref_1h_1p, psi_coef_ref_1h_1p)
 deallocate(psi_det_2h1p, psi_coef_2h1p)
 deallocate(psi_det_1h1p, psi_coef_1h1p)
 deallocate(eigenvectors,eigenvalues)
 deallocate(index_generator)
!return
!

!integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
!integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
!integer(bit_kind), allocatable :: psi_2h1p(:,:,:)
!integer(bit_kind), allocatable :: psi_extra_1h_or_1p(:,:,:)
!double precision, allocatable :: psi_ref_coef_out(:,:)
!double precision, allocatable :: psi_coef_extra_1h_or_1p(:,:)
 
!call all_single_no_1h_or_1p

!call give_n_1h1p_and_n_2h1p_in_psi_det(i_hole,n_det_extra_1h_or_1p,n_det_1h1p,n_det_2h1p)
!allocate(psi_ref_out(N_int,2,N_det_generators))
!allocate(psi_1h1p(N_int,2,n_det_1h1p))
!allocate(psi_2h1p(N_int,2,n_det_2h1p))
!allocate(psi_extra_1h_or_1p(N_int,2,n_det_extra_1h_or_1p))
!allocate(psi_ref_coef_out(N_det_generators,N_states))
!allocate(psi_coef_1h1p(n_det_1h1p,N_states))
!allocate(psi_coef_2h1p(n_det_2h1p,N_states))
!allocate(psi_coef_extra_1h_or_1p(n_det_extra_1h_or_1p,N_states))
!call split_wf_generators_and_1h1p_and_2h1p(i_hole,n_det_extra_1h_or_1p,n_det_1h1p,n_det_2h1p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_2h1p,psi_coef_2h1p,psi_extra_1h_or_1p,psi_coef_extra_1h_or_1p)
!do i = 1, n_det_extra_1h_or_1p
!  print*,'----'
!  print*,'c = ',psi_coef_extra_1h_or_1p(i,1)
!  call debug_det(psi_extra_1h_or_1p(1,1,i),N_int)
!  print*,'----'
!enddo
!call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
!                                                       psi_1h1p,psi_coef_1h1p,n_det_1h1p)
!print*,'Dressing 1h1p '
!do j =1, N_det_generators
! print*,' dressing ',dressing_matrix_1h1p(j,:)
!enddo

!call provide_matrix_dressing_general(dressing_matrix_2h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
!                                                       psi_2h1p,psi_coef_2h1p,n_det_2h1p)
!print*,'Dressing 2h1p '
!do j =1, N_det_generators
! print*,' dressing ',dressing_matrix_2h1p(j,:)
!enddo

!call provide_matrix_dressing_for_extra_1h_or_1p(dressing_matrix_extra_1h_or_1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
!                                                       psi_extra_1h_or_1p,psi_coef_extra_1h_or_1p,n_det_extra_1h_or_1p)
!print*,',dressing_matrix_extra_1h_or_1p'
!do j =1, N_det_generators
! print*,' dressing ',dressing_matrix_extra_1h_or_1p(j,:)
!enddo


!deallocate(psi_ref_out)
!deallocate(psi_1h1p)
!deallocate(psi_2h1p)
!deallocate(psi_extra_1h_or_1p)
!deallocate(psi_ref_coef_out)
!deallocate(psi_coef_1h1p)
!deallocate(psi_coef_2h1p)
!deallocate(psi_coef_extra_1h_or_1p)

end




subroutine all_single_split_for_1h(dressing_matrix_1h1p,dressing_matrix_2h1p)
 implicit none
 use bitmasks
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_2h1p(N_det_generators,N_det_generators)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 integer :: n_det_1h1p,n_det_2h1p
 integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
 integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_2h1p(:,:,:)
 double precision, allocatable :: psi_ref_coef_out(:,:)
 double precision, allocatable :: psi_coef_1h1p(:,:)
 double precision, allocatable :: psi_coef_2h1p(:,:)
 do i = 1, n_inact_orb
   i_hole = list_inact(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   selection_criterion_factor = 1.d-4
   soft_touch selection_criterion_factor selection_criterion selection_criterion_min
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call set_psi_det_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call all_single_no_1h_or_1p
   threshold_davidson = 1.d-10
   soft_touch threshold_davidson davidson_criterion
   call diagonalize_CI
   call give_n_1h1p_and_n_2h1p_in_psi_det(n_det_1h1p,n_det_2h1p)
   allocate(psi_ref_out(N_int,2,N_det_generators))
   allocate(psi_1h1p(N_int,2,n_det_1h1p))
   allocate(psi_2h1p(N_int,2,n_det_2h1p))
   allocate(psi_ref_coef_out(N_det_generators,N_states))
   allocate(psi_coef_1h1p(n_det_1h1p,N_states))
   allocate(psi_coef_2h1p(n_det_2h1p,N_states))
   call split_wf_generators_and_1h1p_and_2h1p(n_det_1h1p,n_det_2h1p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_2h1p,psi_coef_2h1p)
   call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                          psi_1h1p,psi_coef_1h1p,n_det_1h1p)
   call provide_matrix_dressing_general(dressing_matrix_2h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                        psi_2h1p,psi_coef_2h1p,n_det_2h1p)

   deallocate(psi_ref_out)
   deallocate(psi_1h1p)
   deallocate(psi_2h1p)
   deallocate(psi_ref_coef_out)
   deallocate(psi_coef_1h1p)
   deallocate(psi_coef_2h1p)
 enddo
 n_det_max_jacobi = 1000
 soft_touch n_det_max_jacobi
end


subroutine all_single_split_for_1p(dressing_matrix_1h1p,dressing_matrix_1h2p)
 implicit none
 use bitmasks
 double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
 double precision, intent(inout) :: dressing_matrix_1h2p(N_det_generators,N_det_generators)
 integer :: i,i_hole
 n_det_max_jacobi = 50
 soft_touch n_det_max_jacobi

 integer :: n_det_1h1p,n_det_1h2p
 integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
 integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
 integer(bit_kind), allocatable :: psi_1h2p(:,:,:)
 double precision, allocatable :: psi_ref_coef_out(:,:)
 double precision, allocatable :: psi_coef_1h1p(:,:)
 double precision, allocatable :: psi_coef_1h2p(:,:)
 do i = 1, n_inact_orb
   i_hole = list_inact(i)
   print*,''
   print*,'Doing all the single excitations from the orbital '
   print*,i_hole
   print*,''
   print*,''
   threshold_davidson = 1.d-4
   soft_touch threshold_davidson davidson_criterion
   selection_criterion_factor = 1.d-4
   soft_touch selection_criterion_factor selection_criterion selection_criterion_min
   call modify_bitmasks_for_hole(i_hole)
   call set_bitmask_particl_as_input(reunion_of_bitmask)
   call set_generators_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call set_psi_det_as_input_psi(n_det_generators,psi_det_generators,psi_coef_generators)
   call all_single_no_1h_or_1p
   threshold_davidson = 1.d-10
   soft_touch threshold_davidson davidson_criterion
   call diagonalize_CI
   call give_n_1h1p_and_n_1h2p_in_psi_det(n_det_1h1p,n_det_1h2p)
   allocate(psi_ref_out(N_int,2,N_det_generators))
   allocate(psi_1h1p(N_int,2,n_det_1h1p))
   allocate(psi_1h2p(N_int,2,n_det_1h2p))
   allocate(psi_ref_coef_out(N_det_generators,N_states))
   allocate(psi_coef_1h1p(n_det_1h1p,N_states))
   allocate(psi_coef_1h2p(n_det_1h2p,N_states))
   call split_wf_generators_and_1h1p_and_1h2p(n_det_1h1p,n_det_1h2p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_1h2p,psi_coef_1h2p)
   call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                          psi_1h1p,psi_coef_1h1p,n_det_1h1p)
   call provide_matrix_dressing_general(dressing_matrix_1h2p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
                                                          psi_1h2p,psi_coef_1h2p,n_det_1h2p)

   deallocate(psi_ref_out)
   deallocate(psi_1h1p)
   deallocate(psi_1h2p)
   deallocate(psi_ref_coef_out)
   deallocate(psi_coef_1h1p)
   deallocate(psi_coef_1h2p)
 enddo
 n_det_max_jacobi = 1000
 soft_touch n_det_max_jacobi
end

!  subroutine all_single_for_1p(i_particl,dressing_matrix_1h1p,dressing_matrix_1h2p,dressing_matrix_extra_1h_or_1p)
!  implicit none
!  use bitmasks
!  integer, intent(in )            :: i_particl
!  double precision, intent(inout) :: dressing_matrix_1h1p(N_det_generators,N_det_generators)
!  double precision, intent(inout) :: dressing_matrix_1h2p(N_det_generators,N_det_generators)
!  double precision, intent(inout) :: dressing_matrix_extra_1h_or_1p(N_det_generators,N_det_generators)
!  integer :: i
!  n_det_max_jacobi = 50
!  soft_touch n_det_max_jacobi
! 
!  integer :: n_det_1h1p,n_det_1h2p,n_det_extra_1h_or_1p
!  integer(bit_kind), allocatable :: psi_ref_out(:,:,:)
!  integer(bit_kind), allocatable :: psi_1h1p(:,:,:)
!  integer(bit_kind), allocatable :: psi_1h2p(:,:,:)
!  integer(bit_kind), allocatable :: psi_extra_1h_or_1p(:,:,:)
!  double precision, allocatable :: psi_ref_coef_out(:,:)
!  double precision, allocatable :: psi_coef_1h1p(:,:)
!  double precision, allocatable :: psi_coef_1h2p(:,:)
!  double precision, allocatable :: psi_coef_extra_1h_or_1p(:,:)
!!!!call all_single_no_1h_or_1p_or_2p
!  call all_single
!  
!  threshold_davidson = 1.d-12
!  soft_touch threshold_davidson davidson_criterion
!  call diagonalize_CI
!  call give_n_1h1p_and_n_1h2p_in_psi_det(i_particl,n_det_extra_1h_or_1p,n_det_1h1p,n_det_1h2p)
!  allocate(psi_ref_out(N_int,2,N_det_generators))
!  allocate(psi_1h1p(N_int,2,n_det_1h1p))
!  allocate(psi_1h2p(N_int,2,n_det_1h2p))
!  allocate(psi_extra_1h_or_1p(N_int,2,n_det_extra_1h_or_1p))
!  allocate(psi_ref_coef_out(N_det_generators,N_states))
!  allocate(psi_coef_1h1p(n_det_1h1p,N_states))
!  allocate(psi_coef_1h2p(n_det_1h2p,N_states))
!  allocate(psi_coef_extra_1h_or_1p(n_det_extra_1h_or_1p,N_states))
!  call split_wf_generators_and_1h1p_and_1h2p(i_particl,n_det_extra_1h_or_1p,n_det_1h1p,n_det_1h2p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_1h2p,psi_coef_1h2p,psi_extra_1h_or_1p,psi_coef_extra_1h_or_1p)
!  call provide_matrix_dressing_general(dressing_matrix_1h1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
!                                                         psi_1h1p,psi_coef_1h1p,n_det_1h1p)
!  call provide_matrix_dressing_general(dressing_matrix_1h2p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
!                                                         psi_1h2p,psi_coef_1h2p,n_det_1h2p)
!  call provide_matrix_dressing_for_extra_1h_or_1p(dressing_matrix_extra_1h_or_1p,psi_ref_out,psi_ref_coef_out,N_det_generators, &
!                                                         psi_extra_1h_or_1p,psi_coef_extra_1h_or_1p,n_det_extra_1h_or_1p)
! 
!  deallocate(psi_ref_out)
!  deallocate(psi_1h1p)
!  deallocate(psi_1h2p)
!  deallocate(psi_ref_coef_out)
!  deallocate(psi_coef_1h1p)
!  deallocate(psi_coef_1h2p)
! 
!  end


