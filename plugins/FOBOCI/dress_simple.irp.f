
subroutine standard_dress(delta_ij_generators_,size_buffer,Ndet_generators,i_generator,n_selected,det_buffer,Nint,iproc,psi_det_generators_input,E_ref)
 use bitmasks
 implicit none

  integer, intent(in)            :: i_generator,n_selected, Nint, iproc
  integer, intent(in) :: Ndet_generators,size_buffer
  double precision, intent(inout) :: delta_ij_generators_(Ndet_generators,Ndet_generators),E_ref

  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,size_buffer)
  integer(bit_kind), intent(in)  :: psi_det_generators_input(N_int,2,Ndet_generators)
  integer                        :: i,j,k,m
  integer                        :: new_size
  integer                        :: degree(Ndet_generators)
  integer                        :: idx(0:Ndet_generators)
  logical                        :: good

  integer                        :: c_ref
  integer                        :: connected_to_ref


  double precision :: hka, haa
  double precision :: haj
  double precision :: f
  integer :: connected_to_ref_by_mono
  logical :: is_in_wavefunction
  double precision :: H_array(Ndet_generators)
  double precision :: H_matrix_tmp(Ndet_generators+1,Ndet_generators+1)
  double precision :: eigenvectors(Ndet_generators+1,Ndet_generators+1), eigenvalues(Ndet_generators+1)
  double precision :: contrib,lambda_i,accu

  do k = 1, Ndet_generators  
   call i_h_j(psi_det_generators_input(1,1,k),psi_det_generators_input(1,1,k),Nint,hka)
   H_matrix_tmp(k,k) = hka
   do j = k+1, Ndet_generators
    call i_h_j(psi_det_generators_input(1,1,k),psi_det_generators_input(1,1,j),Nint,hka)
    H_matrix_tmp(k,j) = hka
    H_matrix_tmp(j,k) = hka
   enddo
   H_matrix_tmp(k,Ndet_generators+1) = 0.d0
  enddo

  do i=1,n_selected
       c_ref = connected_to_ref_by_mono(det_buffer(1,1,i),psi_det_generators_input,N_int,i_generator,Ndet_generators)
       if (c_ref /= 0) then
         cycle
       endif
       if (is_in_wavefunction(det_buffer(1,1,i),Nint)) then
         cycle
       endif
       call get_excitation_degree_vector(psi_det_generators_input,det_buffer(1,1,i),degree,N_int,Ndet_generators,idx)
       H_array = 0.d0
       do k=1,idx(0)
         call i_h_j(det_buffer(1,1,i),psi_det_generators_input(1,1,idx(k)),Nint,hka)
         H_array(idx(k)) = hka
       enddo
         
       call i_h_j(det_buffer(1,1,i),det_buffer(1,1,i),Nint,haa)
       f = 1.d0/(E_ref-haa)
       
      lambda_i = f
     do k=1,idx(0)
       contrib = H_array(idx(k)) * H_array(idx(k)) * lambda_i
       delta_ij_generators_(idx(k), idx(k)) += contrib
       do j=k+1,idx(0)
         contrib = H_array(idx(k)) * H_array(idx(j)) * lambda_i
         delta_ij_generators_(idx(k), idx(j)) += contrib
         delta_ij_generators_(idx(j), idx(k)) += contrib
       enddo 
     enddo
  enddo
end


subroutine is_a_good_candidate(threshold,is_ok,e_pt2,verbose,exit_loop,is_ok_perturbative)
 use bitmasks
 implicit none
 double precision, intent(in) :: threshold
 double precision, intent(out):: e_pt2
 logical, intent(out) :: is_ok,exit_loop,is_ok_perturbative
 logical, intent(in) :: verbose
 
 integer :: l,k,m
 double precision,allocatable :: dressed_H_matrix(:,:)
 double precision, allocatable :: psi_coef_diagonalized_tmp(:,:)
 integer(bit_kind), allocatable :: psi_det_generators_input(:,:,:)
 double precision :: hij

 allocate(psi_det_generators_input(N_int,2,N_det_generators),dressed_H_matrix(N_det_generators,N_det_generators),psi_coef_diagonalized_tmp(N_det_generators,N_states))
 dressed_H_matrix = 0.d0
 do k = 1, N_det_generators
  do l = 1, N_int
    psi_det_generators_input(l,1,k) = psi_det_generators(l,1,k)
    psi_det_generators_input(l,2,k) = psi_det_generators(l,2,k)
  enddo
 enddo
!call H_apply_dressed_pert(dressed_H_matrix,N_det_generators,psi_det_generators_input)
 call dress_H_matrix_from_psi_det_input(psi_det_generators_input,N_det_generators,is_ok,psi_coef_diagonalized_tmp, dressed_H_matrix,threshold,verbose,exit_loop,is_ok_perturbative)
!do m = 1, N_states
! do k = 1, N_det_generators
!  do l = 1, N_int
!    psi_selectors(l,1,k) = psi_det_generators_input(l,1,k) 
!    psi_selectors(l,2,k) = psi_det_generators_input(l,2,k) 
!  enddo
!  psi_selectors_coef(k,m) = psi_coef_diagonalized_tmp(k,m)
! enddo
!enddo
!soft_touch psi_selectors psi_selectors_coef 
!if(do_it_perturbative)then
   print*, 'is_ok_perturbative',is_ok_perturbative
  if(is_ok.or.is_ok_perturbative)then
   N_det = N_det_generators
   do m = 1, N_states
    do k = 1, N_det_generators
     do l = 1, N_int
       psi_det(l,1,k) = psi_det_generators_input(l,1,k) 
       psi_det(l,2,k) = psi_det_generators_input(l,2,k) 
     enddo
     psi_coef(k,m) = psi_coef_diagonalized_tmp(k,m)
     print*, 'psi_coef(k,m)',psi_coef(k,m)
    enddo
   enddo
   soft_touch psi_det psi_coef  N_det
   e_pt2 = 0.d0
   do m =1, N_det_generators
    do l = 1, N_det_generators
     call i_h_j(psi_det_generators_input(1,1,m),psi_det_generators_input(1,1,l),N_int,hij)  ! Fill the zeroth order H matrix
     e_pt2 += (dressed_H_matrix(m,l) - hij)* psi_coef_diagonalized_tmp(m,1)* psi_coef_diagonalized_tmp(l,1)
    enddo
   enddo
  endif
!endif
 
 deallocate(psi_det_generators_input,dressed_H_matrix,psi_coef_diagonalized_tmp)




end

subroutine dress_H_matrix_from_psi_det_input(psi_det_generators_input,Ndet_generators,is_ok,psi_coef_diagonalized_tmp, dressed_H_matrix,threshold,verbose,exit_loop,is_ok_perturbative)
 use bitmasks
 implicit none
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,Ndet_generators)
 integer, intent(in) :: Ndet_generators
 double precision, intent(in) :: threshold
 logical, intent(in) :: verbose
 logical, intent(out) :: is_ok,exit_loop,is_ok_perturbative
 double precision, intent(out) :: psi_coef_diagonalized_tmp(Ndet_generators,N_states)
 double precision, intent(inout) :: dressed_H_matrix(Ndet_generators, Ndet_generators)

 
 integer :: i,j,degree,index_ref_generators_restart,i_count,k,i_det_no_ref
 double precision :: eigvalues(Ndet_generators), eigvectors(Ndet_generators,Ndet_generators),hij
 double precision :: psi_coef_ref(Ndet_generators,N_states),diag_h_mat_average,diag_h_mat_no_ref_average
 logical :: is_a_ref_det(Ndet_generators)
 exit_loop = .False.
 
 is_a_ref_det = .False.
 do i = 1, N_det_generators
  do j = 1, N_det_generators_restart
   call get_excitation_degree(psi_det_generators_input(1,1,i),psi_det_generators_restart(1,1,j),degree,N_int)  
   if(degree == 0)then
    is_a_ref_det(i) = .True.
    exit
   endif
  enddo
 enddo


 do i = 1, Ndet_generators 
  call get_excitation_degree(ref_generators_restart,psi_det_generators_input(1,1,i),degree,N_int)
  if(degree == 0)then
   index_ref_generators_restart = i
  endif
  do j = 1, Ndet_generators
   call i_h_j(psi_det_generators_input(1,1,j),psi_det_generators_input(1,1,i),N_int,hij)  ! Fill the zeroth order H matrix
   dressed_H_matrix(i,j) = hij
  enddo
 enddo
 i_det_no_ref = 0
 diag_h_mat_average = 0.d0
 do i = 1, Ndet_generators
  if(is_a_ref_det(i))cycle
  i_det_no_ref +=1
  diag_h_mat_average+=dressed_H_matrix(i,i)
 enddo
 diag_h_mat_average = diag_h_mat_average/dble(i_det_no_ref)
 print*,'diag_h_mat_average = ',diag_h_mat_average
 print*,'ref  h_mat         = ',dressed_H_matrix(index_ref_generators_restart,index_ref_generators_restart)
 integer :: number_of_particles, number_of_holes 
 ! Filter the the MLCT that are higher than 27.2 eV in energy with respect to the reference determinant
 do i = 1, Ndet_generators 
  if(is_a_ref_det(i))cycle
  if(number_of_holes(psi_det_generators_input(1,1,i)).eq.0 .and. number_of_particles(psi_det_generators_input(1,1,i)).eq.1)then
   if(diag_h_mat_average - dressed_H_matrix(index_ref_generators_restart,index_ref_generators_restart) .gt.2.d0)then
    is_ok = .False.
    exit_loop = .True.
    return
   endif
  endif
 
  ! Filter the the LMCT that are higher than 54.4 eV in energy with respect to the reference determinant
  if(number_of_holes(psi_det_generators_input(1,1,i)).eq.1 .and. number_of_particles(psi_det_generators_input(1,1,i)).eq.0)then
   if(diag_h_mat_average - dressed_H_matrix(index_ref_generators_restart,index_ref_generators_restart) .gt.2.d0)then
    is_ok = .False.
    return
   endif
  endif
  exit
 enddo

 call lapack_diagd(eigvalues,eigvectors,dressed_H_matrix,Ndet_generators,Ndet_generators)  ! Diagonalize the Dressed_H_matrix
 
 double precision :: s2(N_det_generators),E_ref(N_states)
 integer :: i_state(N_states)
 integer :: n_state_good
 n_state_good = 0
 if(s2_eig)then
  call u_0_S2_u_0(s2,eigvectors,Ndet_generators,psi_det_generators_input,N_int,N_det_generators,size(eigvectors,1))
  do i = 1, Ndet_generators
    print*,'s2 = ',s2(i)
    print*,dabs(s2(i)-expected_s2)
    if(dabs(s2(i)-expected_s2).le.0.3d0)then
     n_state_good +=1
     i_state(n_state_good) = i
     E_ref(n_state_good) = eigvalues(i)
    endif
    if(n_state_good==N_states)then
     exit
    endif
  enddo
 else 
  do i = 1, N_states
   i_state(i) = i
   E_ref(i) = eigvalues(i)
  enddo
 endif
 do i = 1,N_states
  print*,'i_state = ',i_state(i)
 enddo
 do k = 1, N_states
  print*,'state ',k
  do i = 1, Ndet_generators
   psi_coef_diagonalized_tmp(i,k) = eigvectors(i,i_state(k)) / eigvectors(index_ref_generators_restart,i_state(k))
   psi_coef_ref(i,k) = eigvectors(i,i_state(k))
   print*,'psi_coef_ref(i) = ',psi_coef_ref(i,k)
  enddo
 enddo
 if(verbose)then
  print*,'Zeroth order space :'
  do i = 1, Ndet_generators
   write(*,'(10(F16.8),X)')dressed_H_matrix(i,:)
  enddo
  print*,''
  print*,'Zeroth order space Diagonalized :'
  do k = 1, N_states
   print*,'state ',k
   do i = 1, Ndet_generators
    print*,'coef, <I|H|I> = ',psi_coef_diagonalized_tmp(i,k),dressed_H_matrix(i,i)-dressed_H_matrix(index_ref_generators_restart,index_ref_generators_restart),is_a_ref_det(i)
   enddo
  enddo
 endif
 double precision :: E_ref_average
 E_ref_average = 0.d0
 do i = 1, N_states
  E_ref_average += E_ref(i)
 enddo
 E_ref_average = E_ref_average / dble(N_states)

 call H_apply_dressed_pert(dressed_H_matrix,Ndet_generators,psi_det_generators_input,E_ref_average)  !  Calculate the dressing of the H matrix
 if(verbose)then
  print*,'Zeroth order space Dressed by outer space:'
  do i = 1, Ndet_generators
   write(*,'(10(F16.8),X)')dressed_H_matrix(i,:)
  enddo
 endif
 call lapack_diagd(eigvalues,eigvectors,dressed_H_matrix,Ndet_generators,Ndet_generators)  ! Diagonalize the Dressed_H_matrix
 integer :: i_good_state(0:N_states)
 i_good_state(0) = 0
  do i = 1, Ndet_generators
    ! State following
    do k = 1, N_states 
     accu = 0.d0
     do j =1, Ndet_generators
      print*,'',eigvectors(j,i) , psi_coef_ref(j,k)
      accu += eigvectors(j,i) * psi_coef_ref(j,k)
     enddo
     print*,'accu = ',accu
     if(dabs(accu).ge.0.72d0)then
      i_good_state(0) +=1
      i_good_state(i_good_state(0)) = i
     endif
    enddo
    if(i_good_state(0)==N_states)then
     exit
    endif
  enddo
 do i = 1, N_states
  i_state(i) = i_good_state(i)
  E_ref(i) = eigvalues(i_good_state(i))
 enddo
 double precision :: accu
 accu = 0.d0
 do k = 1, N_states
  do i = 1, Ndet_generators
   psi_coef_diagonalized_tmp(i,k) = eigvectors(i,i_state(k)) / eigvectors(index_ref_generators_restart,i_state(k))
  enddo
 enddo
 if(verbose)then
  do k = 1, N_states
   print*,'state ',k
   do i = 1, Ndet_generators
    print*,'coef, <I|H+Delta H|I> = ',psi_coef_diagonalized_tmp(i,k),dressed_H_matrix(i,i)-dressed_H_matrix(index_ref_generators_restart,index_ref_generators_restart),is_a_ref_det(i)
   enddo
  enddo
 endif
 is_ok = .False.
 do i = 1, Ndet_generators
  if(is_a_ref_det(i))cycle
  do k = 1, N_states
   if(dabs(psi_coef_diagonalized_tmp(i,k)) .gt.threshold)then
    is_ok = .True.
    exit
   endif
  enddo
  if(is_ok)then
   exit
  endif
 enddo
 if(.not.is_ok)then
  is_ok_perturbative = .True.
  do i = 1, Ndet_generators
   if(is_a_ref_det(i))cycle
   do k = 1, N_states
    print*, psi_coef_diagonalized_tmp(i,k),threshold_perturbative
    if(dabs(psi_coef_diagonalized_tmp(i,k)) .gt.threshold_perturbative)then
     is_ok_perturbative = .False.
     exit
    endif
   enddo
   if(.not.is_ok_perturbative)then
    exit
   endif
  enddo
 endif
 if(verbose)then
  print*,'is_ok              = ',is_ok
  print*,'is_ok_perturbative = ',is_ok_perturbative
 endif
 

end

subroutine fill_H_apply_buffer_no_selection_first_order_coef(n_selected,det_buffer,Nint,iproc)
  use bitmasks
  implicit none
  BEGIN_DOC
  !  Fill the H_apply buffer with determiants for CISD
  END_DOC
  
  integer, intent(in)            :: n_selected, Nint, iproc
  integer(bit_kind), intent(in)  :: det_buffer(Nint,2,n_selected)
  integer                        :: i,j,k
  integer                        :: new_size
  PROVIDE H_apply_buffer_allocated
  call omp_set_lock(H_apply_buffer_lock(1,iproc))
  new_size = H_apply_buffer(iproc)%N_det + n_selected
  if (new_size > H_apply_buffer(iproc)%sze) then
    call resize_h_apply_buffer(max(2*H_apply_buffer(iproc)%sze,new_size),iproc)
  endif
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
  do i=1,n_selected
    do j=1,N_int
      H_apply_buffer(iproc)%det(j,1,i+H_apply_buffer(iproc)%N_det) = det_buffer(j,1,i)
      H_apply_buffer(iproc)%det(j,2,i+H_apply_buffer(iproc)%N_det) = det_buffer(j,2,i)
    enddo
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i+H_apply_buffer(iproc)%N_det)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i+H_apply_buffer(iproc)%N_det))) == elec_beta_num)
  enddo
  double precision               :: i_H_psi_array(N_states),h,diag_H_mat_elem_fock,delta_e
  do i=1,N_selected
   call i_H_psi(det_buffer(1,1,i),psi_selectors,psi_selectors_coef,N_int,N_det_selectors,psi_selectors_size,N_states,i_H_psi_array)
   call i_H_j(det_buffer(1,1,i),det_buffer(1,1,i),N_int,h)
   do j=1,N_states
      delta_e = -1.d0 /(h - psi_energy(j))
      H_apply_buffer(iproc)%coef(i+H_apply_buffer(iproc)%N_det,j) = i_H_psi_array(j) * delta_e
    enddo
  enddo
  H_apply_buffer(iproc)%N_det = new_size
  do i=1,H_apply_buffer(iproc)%N_det
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,1,i)) )== elec_alpha_num)
    ASSERT (sum(popcnt(H_apply_buffer(iproc)%det(:,2,i))) == elec_beta_num)
  enddo
  call omp_unset_lock(H_apply_buffer_lock(1,iproc))
end



subroutine make_s2_eigenfunction_first_order
  implicit none
  integer                        :: i,j,k
  integer                        :: smax, s
  integer(bit_kind), allocatable :: d(:,:,:), det_buffer(:,:,:)
  integer                        :: N_det_new
  integer, parameter             :: bufsze = 1000
  logical, external              :: is_in_wavefunction

  allocate (d(N_int,2,1), det_buffer(N_int,2,bufsze) )
  smax = 1
  N_det_new = 0

  do i=1,N_occ_pattern
    call occ_pattern_to_dets_size(psi_occ_pattern(1,1,i),s,elec_alpha_num,N_int)
    s += 1
    if (s > smax) then
      deallocate(d)
      allocate ( d(N_int,2,s) )
      smax = s
    endif
    call occ_pattern_to_dets(psi_occ_pattern(1,1,i),d,s,elec_alpha_num,N_int)
    do j=1,s
      if (.not. is_in_wavefunction(d(1,1,j), N_int) ) then
        N_det_new += 1
        do k=1,N_int
          det_buffer(k,1,N_det_new) = d(k,1,j)
          det_buffer(k,2,N_det_new) = d(k,2,j)
        enddo
        if (N_det_new == bufsze) then
          call fill_H_apply_buffer_no_selection(bufsze,det_buffer,N_int,0)
          N_det_new = 0
        endif
      endif
    enddo
  enddo

  if (N_det_new > 0) then
    call fill_H_apply_buffer_no_selection_first_order_coef(N_det_new,det_buffer,N_int,0)
    call copy_H_apply_buffer_to_wf
    SOFT_TOUCH N_det psi_coef psi_det
  endif

  deallocate(d,det_buffer)

   call write_int(output_determinants,N_det_new, 'Added deteminants for S^2')

end

