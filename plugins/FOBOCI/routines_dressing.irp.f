subroutine provide_matrix_dressing(dressing_matrix,ndet_generators_input,psi_det_generators_input)
 use bitmasks
 implicit none
 integer, intent(in) :: ndet_generators_input
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,ndet_generators_input)
 double precision, intent(inout) :: dressing_matrix(ndet_generators_input,ndet_generators_input)
 double precision :: H_array(N_det),hka
 logical :: is_a_ref_det(N_det)
 integer :: i,j,n_det_ref_tmp
 integer :: connected_to_ref_by_mono,degree
 double precision :: coef_ref(Ndet_generators_input)
 double precision :: accu,lambda_i
 integer :: k
 integer :: index_ref_tmp(N_det)
 is_a_ref_det = .False.
 n_det_ref_tmp = 0
 do i = 1, N_det
  do j = 1, Ndet_generators_input
   call get_excitation_degree(psi_det(1,1,i),psi_det_generators_input(1,1,j),degree,N_int)  
   if(degree == 0)then
    is_a_ref_det(i) = .True.
    n_det_ref_tmp +=1
    index_ref_tmp(n_det_ref_tmp) = i
    coef_ref(n_det_ref_tmp) = psi_coef(i,1)
    exit
   endif
  enddo
 enddo
 if( ndet_generators_input .ne. n_det_ref_tmp)then
  print*,'Problem !!!! '
  print*,' ndet_generators .ne. n_det_ref_tmp !!!'
  print*,'ndet_generators,n_det_ref_tmp'
  print*,ndet_generators_input,n_det_ref_tmp 
  stop
 endif
 
 call i_h_j(psi_det_generators_input(1,1,1),psi_det_generators_input(1,1,1),N_int,href)
 integer :: i_pert, i_pert_count
 i_pert_count = 0
 do i = 1, N_det
  if(is_a_ref_det(i))cycle
  call i_h_j(psi_det(1,1,i),psi_det(1,1,i),N_int,hka)
  double precision :: f,href
  f = 1.d0/(href - hka)
  H_array = 0.d0
  accu = 0.d0
  do j=1,ndet_generators_input
   call i_h_j(psi_det(1,1,i),psi_det_generators_input(1,1,j),N_int,hka)
   H_array(j) = hka
   accu += coef_ref(j) * hka
  enddo
  lambda_i = psi_coef(i,1)/accu
  i_pert = 1
  if(accu * f / psi_coef(i,1) .gt. 0.5d0 .and. accu * f/psi_coef(i,1).gt.0.d0)then
   i_pert = 0
  endif
  do j = 1, ndet_generators_input
   if(dabs(H_array(j)*lambda_i).gt.0.1d0)then
    i_pert = 1
    exit
   endif
  enddo
  if(i_pert==1)then
   lambda_i = f
   i_pert_count +=1
  endif
  do k=1,ndet_generators_input
    double precision :: contrib
    contrib = H_array(k) * H_array(k) * lambda_i
    dressing_matrix(k, k) += contrib
    do j=k+1,ndet_generators_input
      contrib = H_array(k) * H_array(j) * lambda_i
      dressing_matrix(k, j) += contrib
      dressing_matrix(j, k) += contrib
    enddo 
  enddo
 enddo
 href = dressing_matrix(1,1)
 print*,'Diagonal part of the dressing'
 do i = 1, ndet_generators_input
  print*,'delta e = ',dressing_matrix(i,i) - href
 enddo
!print*,'i_pert_count = ',i_pert_count
end

subroutine update_matrix_dressing_sc2(dressing_matrix,ndet_generators_input,psi_det_generators_input,H_jj_in)
 use bitmasks
 implicit none
 integer, intent(in) :: ndet_generators_input
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,ndet_generators_input)
 double precision, intent(in)  :: H_jj_in(N_det)
 double precision, intent(inout) :: dressing_matrix(ndet_generators_input,ndet_generators_input)
 integer :: i,j,n_det_ref_tmp,degree
 double precision :: href
 n_det_ref_tmp = 0
 do i = 1, N_det
  do j = 1, Ndet_generators_input
   call get_excitation_degree(psi_det(1,1,i),psi_det_generators_input(1,1,j),degree,N_int)  
   if(degree == 0)then
    dressing_matrix(j,j) += H_jj_in(i)
    n_det_ref_tmp +=1
    exit
   endif
  enddo
 enddo
 if( ndet_generators_input .ne. n_det_ref_tmp)then
  print*,'Problem !!!! '
  print*,' ndet_generators .ne. n_det_ref_tmp !!!'
  print*,'ndet_generators,n_det_ref_tmp'
  print*,ndet_generators_input,n_det_ref_tmp 
  stop
 endif
 
 href = dressing_matrix(1,1)
 print*,''
 print*,'Update with the SC2 dressing'
 print*,''
 print*,'Diagonal part of the dressing'
 do i = 1, ndet_generators_input
  print*,'delta e = ',dressing_matrix(i,i) - href
 enddo
end

subroutine provide_matrix_dressing_for_extra_1h_or_1p(dressing_matrix,psi_det_ref_input,psi_coef_ref_input,n_det_ref_input, &
                                                           psi_det_outer_input,psi_coef_outer_input,n_det_outer_input)
 use bitmasks
 implicit none
 integer, intent(in) :: n_det_ref_input
 integer(bit_kind), intent(in) :: psi_det_ref_input(N_int,2,n_det_ref_input)
 double precision, intent(in) :: psi_coef_ref_input(n_det_ref_input,N_states)
 integer, intent(in) :: n_det_outer_input
 integer(bit_kind), intent(in) :: psi_det_outer_input(N_int,2,n_det_outer_input)
 double precision, intent(in) :: psi_coef_outer_input(n_det_outer_input,N_states)

 double precision, intent(inout) :: dressing_matrix(n_det_ref_input,n_det_ref_input)


 integer :: i_pert, i_pert_count,i,j,k
 double precision :: f,href,hka,lambda_i
 double precision :: H_array(n_det_ref_input),accu
 integer :: n_h_out,n_p_out,n_p_in,n_h_in,number_of_holes,number_of_particles
 call i_h_j(psi_det_ref_input(1,1,1),psi_det_ref_input(1,1,1),N_int,href)
 i_pert_count = 0
 do i = 1, n_det_outer_input
  call i_h_j(psi_det_outer_input(1,1,i),psi_det_outer_input(1,1,i),N_int,hka)
  f = 1.d0/(href - hka)
  H_array = 0.d0
  accu = 0.d0
! n_h_out = number_of_holes(psi_det_outer_input(1,1,i))
! n_p_out = number_of_particles(psi_det_outer_input(1,1,i))
  do j=1,n_det_ref_input
   n_h_in = number_of_holes(psi_det_ref_input(1,1,j))
   n_p_in = number_of_particles(psi_det_ref_input(1,1,j))
!  if(n_h_in == 0 .and. n_h_in == 0)then
    call i_h_j(psi_det_outer_input(1,1,i),psi_det_ref_input(1,1,j),N_int,hka)
!  else 
!   hka = 0.d0
!  endif
   H_array(j) = hka
   accu += psi_coef_ref_input(j,1) * hka
  enddo
  lambda_i = psi_coef_outer_input(i,1)/accu
  i_pert = 1
  if(accu * f / psi_coef_outer_input(i,1) .gt. 0.5d0 .and. accu * f/psi_coef_outer_input(i,1).gt.0.d0)then
   i_pert = 0
  endif
  do j = 1, n_det_ref_input
   if(dabs(H_array(j)*lambda_i).gt.0.5d0)then
    i_pert = 1
    exit
   endif
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! i_pert = 0
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if(i_pert==1)then
   lambda_i = f
   i_pert_count +=1
  endif
  do k=1,n_det_ref_input
    double precision :: contrib
    contrib = H_array(k) * H_array(k) * lambda_i
    dressing_matrix(k, k) += contrib
    do j=k+1,n_det_ref_input
      contrib = H_array(k) * H_array(j) * lambda_i
      dressing_matrix(k, j) += contrib
      dressing_matrix(j, k) += contrib
    enddo 
  enddo
 enddo
end


subroutine provide_matrix_dressing_general(dressing_matrix,psi_det_ref_input,psi_coef_ref_input,n_det_ref_input, &
                                                           psi_det_outer_input,psi_coef_outer_input,n_det_outer_input)
 use bitmasks
 implicit none
 integer, intent(in) :: n_det_ref_input
 integer(bit_kind), intent(in) :: psi_det_ref_input(N_int,2,n_det_ref_input)
 double precision, intent(in) :: psi_coef_ref_input(n_det_ref_input,N_states)
 integer, intent(in) :: n_det_outer_input
 integer(bit_kind), intent(in) :: psi_det_outer_input(N_int,2,n_det_outer_input)
 double precision, intent(in) :: psi_coef_outer_input(n_det_outer_input,N_states)

 double precision, intent(inout) :: dressing_matrix(n_det_ref_input,n_det_ref_input)


 integer :: i_pert, i_pert_count,i,j,k
 double precision :: f,href,hka,lambda_i
 double precision :: H_array(n_det_ref_input),accu
 call i_h_j(psi_det_ref_input(1,1,1),psi_det_ref_input(1,1,1),N_int,href)
 i_pert_count = 0
 do i = 1, n_det_outer_input
  call i_h_j(psi_det_outer_input(1,1,i),psi_det_outer_input(1,1,i),N_int,hka)
  f = 1.d0/(href - hka)
  H_array = 0.d0
  accu = 0.d0
  do j=1,n_det_ref_input
   call i_h_j(psi_det_outer_input(1,1,i),psi_det_ref_input(1,1,j),N_int,hka)
   H_array(j) = hka
   accu += psi_coef_ref_input(j,1) * hka
  enddo
  lambda_i = psi_coef_outer_input(i,1)/accu
  i_pert = 0
  if(accu * f / psi_coef_outer_input(i,1) .gt. 0.5d0 .and. accu * f/psi_coef_outer_input(i,1).gt.0.d0)then
   i_pert = 0
  endif
  do j = 1, n_det_ref_input
   if(dabs(H_array(j)*lambda_i).gt.0.5d0)then
    i_pert = 1
    exit
   endif
  enddo
! i_pert = 0
  if(i_pert==1)then
   lambda_i = f
   i_pert_count +=1
  endif
  do k=1,n_det_ref_input
    double precision :: contrib
    contrib = H_array(k) * H_array(k) * lambda_i
    dressing_matrix(k, k) += contrib
    do j=k+1,n_det_ref_input
      contrib = H_array(k) * H_array(j) * lambda_i
      dressing_matrix(k, j) += contrib
      dressing_matrix(j, k) += contrib
    enddo 
  enddo
 enddo
end


subroutine diag_dressed_matrix_and_set_to_psi_det(psi_det_generators_input,Ndet_generators_input,dressing_matrix)
 use bitmasks
 implicit none
 integer, intent(in) :: ndet_generators_input
 integer(bit_kind), intent(in) :: psi_det_generators_input(N_int,2,ndet_generators_input)
 double precision, intent(inout) :: dressing_matrix(ndet_generators_input,ndet_generators_input)
 integer :: i,j
 
 double precision :: eigenvectors(Ndet_generators_input,Ndet_generators_input), eigenvalues(Ndet_generators_input)

 call lapack_diag(eigenvalues,eigenvectors,dressing_matrix,Ndet_generators_input,Ndet_generators_input)
 print*,'Dressed eigenvalue, to be compared with the CI one'
 print*,'E = ',eigenvalues(1)+nuclear_repulsion
 print*,'Dressed matrix, to be compared to the intermediate Hamiltonian one'
 do i = 1, Ndet_generators_input
  write(*,'(100(F12.5,X))')dressing_matrix(i,:)
 enddo
 n_det = Ndet_generators_input
 do i = 1, Ndet_generators_input
   psi_coef(i,1) = eigenvectors(i,1)
   do j = 1, N_int
    psi_det(j,1,i) = psi_det_generators_input(j,1,i)
    psi_det(j,2,i) = psi_det_generators_input(j,2,i)
   enddo
 enddo

 touch N_det psi_coef psi_det

end

subroutine give_n_1h1p_and_n_2h1p_in_psi_det(i_hole,n_det_extra_1h_or_1p,n_det_1h1p,n_det_2h1p)
 use bitmasks
 implicit none
 integer, intent(in)  :: i_hole
 integer, intent(out) :: n_det_1h1p, n_det_2h1p,n_det_extra_1h_or_1p
 integer :: i
 integer :: n_det_ref_restart_tmp,n_det_1h
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 logical :: is_the_hole_in_det
 n_det_ref_restart_tmp = 0
 n_det_1h = 0
 n_det_1h1p = 0
 n_det_2h1p = 0
 n_det_extra_1h_or_1p = 0
 do i = 1, N_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if(n_h == 0 .and. n_p == 0)then
    n_det_ref_restart_tmp +=1
   else if (n_h ==1 .and. n_p==0)then
    if(is_the_hole_in_det(psi_det(1,1,i),1,i_hole).or.is_the_hole_in_det(psi_det(1,1,i),2,i_hole))then  
     n_det_1h +=1
    else  
     n_det_extra_1h_or_1p +=1
    endif
   else if (n_h ==0 .and. n_p==1)then
    n_det_extra_1h_or_1p +=1
   else if (n_h ==1 .and. n_p==1)then
    n_det_1h1p +=1
   else if (n_h ==2 .and. n_p==1)then
    n_det_2h1p +=1
   else 
    print*,'PB !!!!'
    print*,'You have something else than a 1h, 1p, 1h1p or 2h1p'
    print*,'n_h,n_p = ',n_h,n_p
    call debug_det(psi_det(1,1,i),N_int)
    stop
   endif
  enddo
 if(n_det_ref_restart_tmp + n_det_1h .ne. n_det_generators)then
    print*,'PB !!!!'
    print*,'You have forgotten something in your generators ... '
    stop 
 endif
 if(n_det_2h1p + n_det_1h1p + n_det_extra_1h_or_1p + n_det_generators .ne. N_det)then
    print*,'PB !!!!'
    print*,'You have forgotten something in your generators ... '
    stop 
 endif

end

subroutine give_n_ref_1h_1p_and_n_2h1p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p)
 use bitmasks
 implicit none
 integer, intent(out) :: n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p
 integer :: i
 integer :: n_det_ref_restart_tmp,n_det_1h
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 logical :: is_the_hole_in_det
 n_det_ref_1h_1p = 0
 n_det_2h1p = 0
 n_det_1h1p = 0
 do i = 1, N_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if(n_h == 0 .and. n_p == 0)then
    n_det_ref_1h_1p +=1
   else if (n_h ==1 .and. n_p==0)then
     n_det_ref_1h_1p +=1
   else if (n_h ==0 .and. n_p==1)then
    n_det_ref_1h_1p +=1
   else if (n_h ==1 .and. n_p==1)then
    n_det_1h1p +=1
   else if (n_h ==2 .and. n_p==1)then
    n_det_2h1p +=1
   else 
    print*,'PB !!!!'
    print*,'You have something else than a 1h, 1p, 1h1p or 2h1p'
    print*,'n_h,n_p = ',n_h,n_p
    call debug_det(psi_det(1,1,i),N_int)
    stop
   endif
  enddo

end

subroutine give_n_ref_1h_1p_and_n_1h2p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p)
 use bitmasks
 implicit none
 integer, intent(out) :: n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p
 integer :: i
 integer :: n_det_ref_restart_tmp,n_det_1h
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 logical :: is_the_hole_in_det
 n_det_ref_1h_1p = 0
 n_det_1h2p = 0
 n_det_1h1p = 0
 do i = 1, N_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if(n_h == 0 .and. n_p == 0)then
    n_det_ref_1h_1p +=1
   else if (n_h ==1 .and. n_p==0)then
     n_det_ref_1h_1p +=1
   else if (n_h ==0 .and. n_p==1)then
    n_det_ref_1h_1p +=1
   else if (n_h ==1 .and. n_p==1)then
    n_det_1h1p +=1
   else if (n_h ==1 .and. n_p==2)then
    n_det_1h2p +=1
   else 
    print*,'PB !!!!'
    print*,'You have something else than a 1h, 1p, 1h1p or 1h2p'
    print*,'n_h,n_p = ',n_h,n_p
    call debug_det(psi_det(1,1,i),N_int)
    stop
   endif
  enddo

end

subroutine give_wf_n_ref_1h_1p_and_n_2h1p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,& 
                                                          psi_det_2h1p,psi_coef_2h1p,psi_det_1h1p,psi_coef_1h1p)
 use bitmasks
 implicit none
 integer, intent(in) :: n_det_ref_1h_1p,n_det_2h1p,n_det_1h1p
 integer(bit_kind), intent(out) :: psi_det_ref_1h_1p(N_int,2,n_det_ref_1h_1p)
 integer(bit_kind), intent(out) :: psi_det_2h1p(N_int,2,n_det_2h1p)
 integer(bit_kind), intent(out) :: psi_det_1h1p(N_int,2,n_det_1h1p)
 double precision, intent(out)  :: psi_coef_ref_1h_1p(n_det_ref_1h_1p,N_states)
 double precision, intent(out)  :: psi_coef_2h1p(n_det_2h1p,N_states)
 double precision, intent(out)  :: psi_coef_1h1p(n_det_1h1p,N_states)
 integer :: n_det_ref_1h_1p_tmp,n_det_2h1p_tmp,n_det_1h1p_tmp
 integer :: i,j
 integer :: n_det_ref_restart_tmp,n_det_1h
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 logical :: is_the_hole_in_det
 integer, allocatable :: index_ref_1h_1p(:)
 integer, allocatable :: index_2h1p(:)
 integer, allocatable :: index_1h1p(:)
 allocate(index_ref_1h_1p(n_det))
 allocate(index_2h1p(n_det))
 allocate(index_1h1p(n_det))
 n_det_ref_1h_1p_tmp = 0
 n_det_2h1p_tmp = 0
 n_det_1h1p_tmp = 0
 do i = 1, N_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if(n_h == 0 .and. n_p == 0)then
    n_det_ref_1h_1p_tmp +=1
    index_ref_1h_1p(n_det_ref_1h_1p_tmp) = i
   else if (n_h ==1 .and. n_p==0)then
    n_det_ref_1h_1p_tmp +=1
    index_ref_1h_1p(n_det_ref_1h_1p_tmp) = i
   else if (n_h ==0 .and. n_p==1)then
    n_det_ref_1h_1p_tmp +=1
    index_ref_1h_1p(n_det_ref_1h_1p_tmp) = i
   else if (n_h ==1 .and. n_p==1)then
    n_det_1h1p_tmp +=1
    index_1h1p(n_det_1h1p_tmp) = i
   else if (n_h ==2 .and. n_p==1)then
    n_det_2h1p_tmp +=1
    index_2h1p(n_det_2h1p_tmp) = i
   else 
    print*,'PB !!!!'
    print*,'You have something else than a 1h, 1p, 1h1p or 2h1p'
    print*,'n_h,n_p = ',n_h,n_p
    call debug_det(psi_det(1,1,i),N_int)
    stop
   endif
  enddo
 do i = 1, n_det_2h1p
  do j = 1, N_int
   psi_det_2h1p(j,1,i) = psi_det(j,1,index_2h1p(i))
   psi_det_2h1p(j,2,i) = psi_det(j,2,index_2h1p(i))
  enddo
  do j = 1, N_states
   psi_coef_2h1p(i,j) = psi_coef(index_2h1p(i),j)
  enddo
 enddo

 do i = 1, n_det_1h1p
  do j = 1, N_int
   psi_det_1h1p(j,1,i) = psi_det(j,1,index_1h1p(i))
   psi_det_1h1p(j,2,i) = psi_det(j,2,index_1h1p(i))
  enddo
  do j = 1, N_states
   psi_coef_1h1p(i,j) = psi_coef(index_1h1p(i),j)
  enddo
 enddo

 do i = 1, n_det_ref_1h_1p
  do j = 1, N_int
   psi_det_ref_1h_1p(j,1,i) = psi_det(j,1,index_ref_1h_1p(i))
   psi_det_ref_1h_1p(j,2,i) = psi_det(j,2,index_ref_1h_1p(i))
  enddo
  do j = 1, N_states
   psi_coef_ref_1h_1p(i,j) = psi_coef(index_ref_1h_1p(i),j)
  enddo
 enddo

end

subroutine give_wf_n_ref_1h_1p_and_n_1h2p_1h1p_in_psi_det(n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p,psi_det_ref_1h_1p,psi_coef_ref_1h_1p,& 
                                                          psi_det_1h2p,psi_coef_1h2p,psi_det_1h1p,psi_coef_1h1p)
 use bitmasks
 implicit none
 integer, intent(in) :: n_det_ref_1h_1p,n_det_1h2p,n_det_1h1p
 integer(bit_kind), intent(out) :: psi_det_ref_1h_1p(N_int,2,n_det_ref_1h_1p)
 integer(bit_kind), intent(out) :: psi_det_1h2p(N_int,2,n_det_1h2p)
 integer(bit_kind), intent(out) :: psi_det_1h1p(N_int,2,n_det_1h1p)
 double precision, intent(out)  :: psi_coef_ref_1h_1p(n_det_ref_1h_1p,N_states)
 double precision, intent(out)  :: psi_coef_1h2p(n_det_1h2p,N_states)
 double precision, intent(out)  :: psi_coef_1h1p(n_det_1h1p,N_states)
 integer :: n_det_ref_1h_1p_tmp,n_det_1h2p_tmp,n_det_1h1p_tmp
 integer :: i,j
 integer :: n_det_ref_restart_tmp,n_det_1h
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 logical :: is_the_hole_in_det
 integer, allocatable :: index_ref_1h_1p(:)
 integer, allocatable :: index_1h2p(:)
 integer, allocatable :: index_1h1p(:)
 allocate(index_ref_1h_1p(n_det))
 allocate(index_1h2p(n_det))
 allocate(index_1h1p(n_det))
 n_det_ref_1h_1p_tmp = 0
 n_det_1h2p_tmp = 0
 n_det_1h1p_tmp = 0
 do i = 1, N_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if(n_h == 0 .and. n_p == 0)then
    n_det_ref_1h_1p_tmp +=1
    index_ref_1h_1p(n_det_ref_1h_1p_tmp) = i
   else if (n_h ==1 .and. n_p==0)then
    n_det_ref_1h_1p_tmp +=1
    index_ref_1h_1p(n_det_ref_1h_1p_tmp) = i
   else if (n_h ==0 .and. n_p==1)then
    n_det_ref_1h_1p_tmp +=1
    index_ref_1h_1p(n_det_ref_1h_1p_tmp) = i
   else if (n_h ==1 .and. n_p==1)then
    n_det_1h1p_tmp +=1
    index_1h1p(n_det_1h1p_tmp) = i
   else if (n_h ==1 .and. n_p==2)then
    n_det_1h2p_tmp +=1
    index_1h2p(n_det_1h2p_tmp) = i
   else 
    print*,'PB !!!!'
    print*,'You have something else than a 1h, 1p, 1h1p or 1h2p'
    print*,'n_h,n_p = ',n_h,n_p
    call debug_det(psi_det(1,1,i),N_int)
    stop
   endif
  enddo
 do i = 1, n_det_1h2p
  do j = 1, N_int
   psi_det_1h2p(j,1,i) = psi_det(j,1,index_1h2p(i))
   psi_det_1h2p(j,2,i) = psi_det(j,2,index_1h2p(i))
  enddo
  do j = 1, N_states
   psi_coef_1h2p(i,j) = psi_coef(index_1h2p(i),j)
  enddo
 enddo

 do i = 1, n_det_1h1p
  do j = 1, N_int
   psi_det_1h1p(j,1,i) = psi_det(j,1,index_1h1p(i))
   psi_det_1h1p(j,2,i) = psi_det(j,2,index_1h1p(i))
  enddo
  do j = 1, N_states
   psi_coef_1h1p(i,j) = psi_coef(index_1h1p(i),j)
  enddo
 enddo

 do i = 1, n_det_ref_1h_1p
  do j = 1, N_int
   psi_det_ref_1h_1p(j,1,i) = psi_det(j,1,index_ref_1h_1p(i))
   psi_det_ref_1h_1p(j,2,i) = psi_det(j,2,index_ref_1h_1p(i))
  enddo
  do j = 1, N_states
   psi_coef_ref_1h_1p(i,j) = psi_coef(index_ref_1h_1p(i),j)
  enddo
 enddo

end



subroutine give_n_1h1p_and_n_1h2p_in_psi_det(i_particl,n_det_extra_1h_or_1p,n_det_1h1p,n_det_1h2p)
 use bitmasks
 implicit none
 integer, intent(in)  ::i_particl
 integer, intent(out) :: n_det_1h1p, n_det_1h2p,n_det_extra_1h_or_1p
 integer :: i
 integer :: n_det_ref_restart_tmp,n_det_1p
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 logical :: is_the_particl_in_det
 n_det_ref_restart_tmp = 0
 n_det_1p = 0
 n_det_1h1p = 0
 n_det_1h2p = 0
 n_det_extra_1h_or_1p = 0
 do i = 1, N_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if(n_h == 0 .and. n_p == 0)then
    n_det_ref_restart_tmp +=1
   else if (n_h ==0 .and. n_p==1)then
    if(is_the_particl_in_det(psi_det(1,1,i),1,i_particl).or.is_the_particl_in_det(psi_det(1,1,i),2,i_particl))then
     n_det_1p +=1
    else 
     n_det_extra_1h_or_1p +=1
    endif
   else if (n_h ==1 .and. n_p==0)then
    n_det_extra_1h_or_1p +=1
   else if (n_h ==1 .and. n_p==1)then
    n_det_1h1p +=1
   else if (n_h ==1 .and. n_p==2)then
    n_det_1h2p +=1
   else 
    print*,'PB !!!!'
    print*,'You have something else than a 1h, 1p, 1h1p or 1h2p'
    call debug_det(psi_det(1,1,i),N_int)
    stop
   endif
  enddo
!if(n_det_ref_restart_tmp + n_det_1h .ne. n_det_generators)then
!   print*,'PB !!!!'
!   print*,'You have forgotten something in your generators ... '
!   stop 
!endif


end


subroutine split_wf_generators_and_1h1p_and_2h1p(i_hole,n_det_extra_1h_or_1p,n_det_1h1p,n_det_2h1p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_2h1p,psi_coef_2h1p,psi_extra_1h_or_1p,psi_coef_extra_1h_or_1p)
 use bitmasks
 implicit none
 integer, intent(in) :: n_det_1h1p,n_det_2h1p,n_det_extra_1h_or_1p,i_hole
 integer(bit_kind), intent(out) :: psi_ref_out(N_int,2,N_det_generators)
 integer(bit_kind), intent(out) :: psi_1h1p(N_int,2,n_det_1h1p)
 integer(bit_kind), intent(out) :: psi_2h1p(N_int,2,n_det_2h1p)
 integer(bit_kind), intent(out) :: psi_extra_1h_or_1p(N_int,2,n_det_extra_1h_or_1p)
 double precision,  intent(out) :: psi_ref_coef_out(N_det_generators,N_states)
 double precision,  intent(out) :: psi_coef_1h1p(n_det_1h1p, N_states)
 double precision,  intent(out) :: psi_coef_2h1p(n_det_2h1p, N_states)
 double precision,  intent(out) :: psi_coef_extra_1h_or_1p(n_det_extra_1h_or_1p, N_states)

 integer :: i,j
 integer :: degree
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 integer :: n_det_generators_tmp,n_det_1h1p_tmp,n_det_2h1p_tmp,n_det_extra_1h_or_1p_tmp
 integer :: n_det_1h_tmp
 integer, allocatable  :: index_generator(:)
 integer, allocatable :: index_1h1p(:)
 integer, allocatable :: index_2h1p(:)
 integer, allocatable :: index_extra_1h_or_1p(:)
 logical :: is_the_hole_in_det

 allocate(index_1h1p(n_det))
 allocate(index_2h1p(n_det))
 allocate(index_extra_1h_or_1p(n_det))
 allocate(index_generator(N_det))


 n_det_generators_tmp = 0
 n_det_1h1p_tmp = 0
 n_det_2h1p_tmp = 0
 n_det_extra_1h_or_1p_tmp = 0
 n_det_1h_tmp = 0
 do i = 1, n_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if (n_h ==1 .and. n_p==1)then
    n_det_1h1p_tmp +=1
    index_1h1p(n_det_1h1p_tmp) = i
   else if (n_h ==2 .and. n_p==1)then
    n_det_2h1p_tmp +=1
    index_2h1p(n_det_2h1p_tmp) = i
   else if (n_h ==0 .and. n_p==1)then
    n_det_extra_1h_or_1p_tmp +=1
    index_extra_1h_or_1p(n_det_extra_1h_or_1p_tmp) = i
   else if (n_h ==1 .and. n_p==0)then
    if(is_the_hole_in_det(psi_det(1,1,i),1,i_hole).or.is_the_hole_in_det(psi_det(1,1,i),2,i_hole))then  
     n_det_1h_tmp +=1
    else
     n_det_extra_1h_or_1p_tmp +=1
     index_extra_1h_or_1p(n_det_extra_1h_or_1p_tmp) = i
    endif
   endif
   do j = 1, N_det_generators
    call get_excitation_degree(psi_det_generators(1,1,j),psi_det(1,1,i), degree, N_int)
    if(degree == 0)then
     n_det_generators_tmp +=1
     index_generator(n_det_generators_tmp) = i
    endif
   enddo
 enddo
 if(n_det_1h1p_tmp.ne.n_det_1h1p)then
  print*,'PB !!!'
  print*,'n_det_1h1p_tmp.ne.n_det_1h1p)'
  stop
 endif


 if(n_det_2h1p_tmp.ne.n_det_2h1p)then
  print*,'PB !!!'
  print*,'n_det_2h1p_tmp.ne.n_det_2h1p)'
  stop
 endif

 if(N_det_generators_tmp.ne.n_det_generators)then
  print*,'PB !!!'
  print*,'N_det_generators_tmp.ne.n_det_generators'
  stop
 endif

 if(n_det_extra_1h_or_1p.ne.n_det_extra_1h_or_1p_tmp)then
  print*,'PB !!!'
  print*,'n_det_extra_1h_or_1p.ne.n_det_extra_1h_or_1p_tmp'
  stop
 endif

 do i = 1,N_det_generators
  do j = 1, N_int
   psi_ref_out(j,1,i) = psi_det(j,1,index_generator(i))
   psi_ref_out(j,2,i) = psi_det(j,2,index_generator(i))
  enddo
  do j = 1, N_states
   psi_ref_coef_out(i,j) = psi_coef(index_generator(i),j)
  enddo
 enddo

 do i = 1, n_det_1h1p
  do j = 1, N_int
   psi_1h1p(j,1,i) = psi_det(j,1,index_1h1p(i))
   psi_1h1p(j,2,i) = psi_det(j,2,index_1h1p(i))
  enddo
  do j = 1, N_states
   psi_coef_1h1p(i,j) = psi_coef(index_1h1p(i),j)
  enddo
 enddo

 do i = 1, n_det_2h1p
  do j = 1, N_int
   psi_2h1p(j,1,i) = psi_det(j,1,index_2h1p(i))
   psi_2h1p(j,2,i) = psi_det(j,2,index_2h1p(i))
  enddo
  do j = 1, N_states
   psi_coef_2h1p(i,j) = psi_coef(index_2h1p(i),j)
  enddo
 enddo

 do i = 1, n_det_extra_1h_or_1p
  do j = 1, N_int
   psi_extra_1h_or_1p(j,1,i) = psi_det(j,1,index_extra_1h_or_1p(i))
   psi_extra_1h_or_1p(j,2,i) = psi_det(j,2,index_extra_1h_or_1p(i))
  enddo
  do j = 1, N_states
   psi_coef_extra_1h_or_1p(i,j) = psi_coef(index_extra_1h_or_1p(i),j)
  enddo
 enddo

 deallocate(index_generator)
 deallocate(index_1h1p)
 deallocate(index_2h1p)
 deallocate(index_extra_1h_or_1p)

end


subroutine split_wf_generators_and_1h1p_and_1h2p(i_particl,n_det_extra_1h_or_1p,n_det_1h1p,n_det_1h2p,psi_ref_out,psi_ref_coef_out,psi_1h1p,psi_coef_1h1p,psi_1h2p,psi_coef_1h2p,psi_extra_1h_or_1p,psi_coef_extra_1h_or_1p)
 use bitmasks
 implicit none
 integer, intent(in) :: n_det_1h1p,n_det_1h2p,n_det_extra_1h_or_1p,i_particl
 integer(bit_kind), intent(out) :: psi_ref_out(N_int,2,N_det_generators)
 integer(bit_kind), intent(out) :: psi_1h1p(N_int,2,n_det_1h1p)
 integer(bit_kind), intent(out) :: psi_1h2p(N_int,2,n_det_1h2p)
 integer(bit_kind), intent(out) :: psi_extra_1h_or_1p(N_int,2,n_det_extra_1h_or_1p)
 double precision,  intent(out) :: psi_ref_coef_out(N_det_generators,N_states)
 double precision,  intent(out) :: psi_coef_1h1p(n_det_1h1p, N_states)
 double precision,  intent(out) :: psi_coef_1h2p(n_det_1h2p, N_states)
 double precision,  intent(out) :: psi_coef_extra_1h_or_1p(n_det_extra_1h_or_1p, N_states)

 integer :: i,j
 integer :: degree
 integer ::  number_of_holes,n_h, number_of_particles,n_p
 integer :: n_det_generators_tmp,n_det_1h1p_tmp,n_det_1h2p_tmp,n_det_extra_1h_or_1p_tmp
 integer, allocatable  :: index_generator(:)
 integer, allocatable :: index_1h1p(:)
 integer, allocatable :: index_1h2p(:)
 integer, allocatable :: index_extra_1h_or_1p(:)
 logical :: is_the_particl_in_det
 integer :: n_det_1p_tmp

 allocate(index_1h1p(n_det))
 allocate(index_1h2p(n_det))
 allocate(index_extra_1h_or_1p(n_det))
 allocate(index_generator(N_det))


 n_det_generators_tmp = 0
 n_det_1h1p_tmp = 0
 n_det_1h2p_tmp = 0
 n_det_extra_1h_or_1p_tmp = 0
 n_det_1p_tmp = 0
 do i = 1, n_det
   n_h = number_of_holes(psi_det(1,1,i))
   n_p = number_of_particles(psi_det(1,1,i))
   if (n_h ==1 .and. n_p==1)then
    n_det_1h1p_tmp +=1
    index_1h1p(n_det_1h1p_tmp) = i
   else if (n_h ==1 .and. n_p==2)then
    n_det_1h2p_tmp +=1
    index_1h2p(n_det_1h2p_tmp) = i
   else if (n_h ==1 .and. n_p==0)then
    n_det_extra_1h_or_1p_tmp +=1
    index_extra_1h_or_1p(n_det_extra_1h_or_1p_tmp) = i
   else if (n_h ==0 .and. n_p==1)then
    if(is_the_particl_in_det(psi_det(1,1,i),1,i_particl).or.is_the_particl_in_det(psi_det(1,1,i),2,i_particl))then
     n_det_1p_tmp +=1
    else 
     n_det_extra_1h_or_1p_tmp +=1
    endif
   endif
   do j = 1, N_det_generators
    call get_excitation_degree(psi_det_generators(1,1,j),psi_det(1,1,i), degree, N_int)
    if(degree == 0)then
     n_det_generators_tmp +=1
     index_generator(n_det_generators_tmp) = i
    endif
   enddo
 enddo
 if(n_det_1h1p_tmp.ne.n_det_1h1p)then
  print*,'PB !!!'
  print*,'n_det_1h1p_tmp.ne.n_det_1h1p)'
  stop
 endif


 if(n_det_1h2p_tmp.ne.n_det_1h2p)then
  print*,'PB !!!'
  print*,'n_det_1h2p_tmp.ne.n_det_1h2p)'
  stop
 endif

 if(N_det_generators_tmp.ne.n_det_generators)then
  print*,'PB !!!'
  print*,'N_det_generators_tmp.ne.n_det_generators'
  stop
 endif

 do i = 1,N_det_generators
  do j = 1, N_int
   psi_ref_out(j,1,i) = psi_det(j,1,index_generator(i))
   psi_ref_out(j,2,i) = psi_det(j,2,index_generator(i))
  enddo
  do j = 1, N_states
   psi_ref_coef_out(i,j) = psi_coef(index_generator(i),j)
  enddo
 enddo

 do i = 1, n_det_1h1p
  do j = 1, N_int
   psi_1h1p(j,1,i) = psi_det(j,1,index_1h1p(i))
   psi_1h1p(j,2,i) = psi_det(j,2,index_1h1p(i))
  enddo
  do j = 1, N_states
   psi_coef_1h1p(i,j) = psi_coef(index_1h1p(i),j)
  enddo
 enddo

 do i = 1, n_det_1h2p
  do j = 1, N_int
   psi_1h2p(j,1,i) = psi_det(j,1,index_1h2p(i))
   psi_1h2p(j,2,i) = psi_det(j,2,index_1h2p(i))
  enddo
  do j = 1, N_states
   psi_coef_1h2p(i,j) = psi_coef(index_1h2p(i),j)
  enddo
 enddo


 do i = 1, n_det_extra_1h_or_1p
  do j = 1, N_int
   psi_extra_1h_or_1p(j,1,i) = psi_det(j,1,index_extra_1h_or_1p(i))
   psi_extra_1h_or_1p(j,2,i) = psi_det(j,2,index_extra_1h_or_1p(i))
  enddo
  do j = 1, N_states
   psi_coef_extra_1h_or_1p(i,j) = psi_coef(index_extra_1h_or_1p(i),j)
  enddo
 enddo

 deallocate(index_generator)
 deallocate(index_1h1p)
 deallocate(index_1h2p)
 deallocate(index_extra_1h_or_1p)

end


