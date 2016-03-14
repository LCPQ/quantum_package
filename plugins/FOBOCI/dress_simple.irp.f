
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
       
     if(second_order_h)then
      lambda_i = f
     else
       ! You write the new Hamiltonian matrix
       do k = 1, Ndet_generators
        H_matrix_tmp(k,Ndet_generators+1) = H_array(k)
        H_matrix_tmp(Ndet_generators+1,k) = H_array(k)
       enddo
       H_matrix_tmp(Ndet_generators+1,Ndet_generators+1) = haa
       ! Then diagonalize it
       call lapack_diag(eigenvalues,eigenvectors,H_matrix_tmp,Ndet_generators+1,Ndet_generators+1)
       ! Then you extract the effective denominator
       accu = 0.d0
       do k = 1, Ndet_generators
        accu += eigenvectors(k,1) * H_array(k)
       enddo
       lambda_i = eigenvectors(Ndet_generators+1,1)/accu
     endif
     do k=1,idx(0)
       contrib = H_array(idx(k)) * H_array(idx(k)) * lambda_i
       delta_ij_generators_(idx(k), idx(k)) += contrib
       do j=k+1,idx(0)
         contrib = H_array(idx(k)) * H_array(idx(j)) * lambda_i
         delta_ij_generators_(idx(k), idx(j)) += contrib
         delta_ij_generators_(idx(j), idx(k)) += contrib
       enddo 
     enddo
!      H_matrix_tmp_bis(idx(k),idx(k)) += contrib
!        H_matrix_tmp_bis(idx(k),idx(j)) += contrib
!        H_matrix_tmp_bis(idx(j),idx(k)) += contrib
!    do k = 1, Ndet_generators
!     do j = 1, Ndet_generators
!      H_matrix_tmp_bis(k,j) = H_matrix_tmp(k,j)
!     enddo
!    enddo
!    double precision :: H_matrix_tmp_bis(Ndet_generators,Ndet_generators)
!    double precision :: eigenvectors_bis(Ndet_generators,Ndet_generators), eigenvalues_bis(Ndet_generators)
!    call lapack_diag(eigenvalues_bis,eigenvectors_bis,H_matrix_tmp_bis,Ndet_generators,Ndet_generators)
!    print*,'f,lambda_i = ',f,lambda_i
!    print*,'eigenvalues_bi(1)',eigenvalues_bis(1)
!    print*,'eigenvalues      ',eigenvalues(1)
!    do k = 1, Ndet_generators
!     print*,'coef,coef_dres = ', eigenvectors(k,1), eigenvectors_bis(k,1)
!    enddo
!    pause
!    accu = 0.d0
!    do k = 1, Ndet_generators
!     do j = 1, Ndet_generators
!      accu += eigenvectors(k,1) * eigenvectors(j,1) * (H_matrix_tmp(k,j) + delta_ij_generators_(k,j))
!     enddo
!    enddo
!    print*,'accu,eigv = ',accu,eigenvalues(1)
!    pause

  enddo
end


subroutine is_a_good_candidate(threshold,is_ok,verbose)
 use bitmasks
 implicit none
 double precision, intent(in) :: threshold
 logical, intent(out) :: is_ok
 logical, intent(in) :: verbose
 
 integer :: l,k,m
 double precision,allocatable :: dressed_H_matrix(:,:)
 double precision,allocatable :: psi_coef_diagonalized_tmp(:,:)
 integer(bit_kind), allocatable :: psi_det_generators_input(:,:,:)

 allocate(psi_det_generators_input(N_int,2,N_det_generators),dressed_H_matrix(N_det_generators,N_det_generators))
 allocate(psi_coef_diagonalized_tmp(N_det_generators,N_states))
 dressed_H_matrix = 0.d0
 do k = 1, N_det_generators
  do l = 1, N_int
    psi_det_generators_input(l,1,k) = psi_det_generators(l,1,k)
    psi_det_generators_input(l,2,k) = psi_det_generators(l,2,k)
  enddo
 enddo
!call H_apply_dressed_pert(dressed_H_matrix,N_det_generators,psi_det_generators_input)
 call dress_H_matrix_from_psi_det_input(psi_det_generators_input,N_det_generators,is_ok,psi_coef_diagonalized_tmp, dressed_H_matrix,threshold,verbose)
 if(do_it_perturbative)then
  if(is_ok)then
   N_det = N_det_generators
   do m = 1, N_states
    do k = 1, N_det_generators
     do l = 1, N_int
       psi_det(l,1,k) = psi_det_generators_input(l,1,k) 
       psi_det(l,2,k) = psi_det_generators_input(l,2,k) 
     enddo
     psi_coef(k,m) = psi_coef_diagonalized_tmp(k,m)
    enddo
   enddo
   touch psi_coef psi_det N_det
  endif
 endif
 
 deallocate(psi_det_generators_input,dressed_H_matrix,psi_coef_diagonalized_tmp)




end

subroutine dress_H_matrix_from_psi_det_input(psi_det_generators_input,Ndet_generators,is_ok,psi_coef_diagonalized_tmp, dressed_H_matrix,threshold,verbose)
 use bitmasks
 implicit none
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,Ndet_generators)
 integer, intent(in) :: Ndet_generators
 double precision, intent(in) :: threshold
 logical, intent(in) :: verbose
 logical, intent(out) :: is_ok
 double precision, intent(out) :: psi_coef_diagonalized_tmp(Ndet_generators,N_states)
 double precision, intent(inout) :: dressed_H_matrix(Ndet_generators, Ndet_generators)

 
 integer :: i,j,degree,index_ref_generators_restart,i_count,k,i_det_no_ref
 double precision :: eigvalues(Ndet_generators), eigvectors(Ndet_generators,Ndet_generators),hij
 double precision :: psi_coef_ref(Ndet_generators,N_states),diag_h_mat_average,diag_h_mat_no_ref_average
 logical :: is_a_ref_det(Ndet_generators)
 
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
 
 double precision :: s2,E_ref(N_states)
 integer :: i_state(N_states)
 integer :: n_state_good
 n_state_good = 0
 if(s2_eig)then
  do i = 1, Ndet_generators
    call get_s2_u0(psi_det_generators_input,eigvectors(1,i),Ndet_generators,Ndet_generators,s2)
    print*,'s2 = ',s2
    print*,dabs(s2-expected_s2)
    if(dabs(s2-expected_s2).le.0.3d0)then
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
    call get_s2_u0(psi_det_generators_input,eigvectors(1,i),Ndet_generators,Ndet_generators,s2)
    ! State following
    do k = 1, N_states 
     accu = 0.d0
     do j =1, Ndet_generators
      accu += eigvectors(j,i) * psi_coef_ref(j,k)
     enddo
     if(dabs(accu).ge.0.8d0)then
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
 if(verbose)then
  print*,'is_ok = ',is_ok
 endif
 

end

